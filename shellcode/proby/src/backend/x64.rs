pub mod mm {
    use iced_x86::code_asm::*;
    pub trait MM: Sized + Copy {
        fn all() -> [Self; 32];
    }
    impl MM for AsmRegisterXmm {
        fn all() -> [Self; 32] {
            [
                xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12,
                xmm13, xmm14, xmm15, xmm16, xmm17, xmm18, xmm19, xmm20, xmm21, xmm22, xmm23, xmm24,
                xmm25, xmm26, xmm27, xmm28, xmm29, xmm30, xmm31,
            ]
        }
    }
    impl MM for AsmRegisterYmm {
        fn all() -> [Self; 32] {
            [
                ymm0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6, ymm7, ymm8, ymm9, ymm10, ymm11, ymm12,
                ymm13, ymm14, ymm15, ymm16, ymm17, ymm18, ymm19, ymm20, ymm21, ymm22, ymm23, ymm24,
                ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31,
            ]
        }
    }
    impl MM for AsmRegisterZmm {
        fn all() -> [Self; 32] {
            [
                zmm0, zmm1, zmm2, zmm3, zmm4, zmm5, zmm6, zmm7, zmm8, zmm9, zmm10, zmm11, zmm12,
                zmm13, zmm14, zmm15, zmm16, zmm17, zmm18, zmm19, zmm20, zmm21, zmm22, zmm23, zmm24,
                zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31,
            ]
        }
    }
}
pub mod regalloc {
    use iced_x86::code_asm::*;
    use rand::prelude::SliceRandom;
    use rand::Rng;
    pub struct Backend<R> {
        all: Vec<AsmRegister64>,
        pub rng: R,
    }
    pub struct Token {
        reg: AsmRegister64,
    }
    impl Token {
        pub fn reg(&self) -> AsmRegister64 {
            return self.reg;
        }
        pub fn unused<R: Rng, T>(
            &mut self,
            r: &mut Backend<R>,
            go: impl FnOnce(&mut Backend<R>) -> T,
        ) -> T {
            let i = r.all.iter().position(|a| *a == self.reg).unwrap();
            r.all[i] = rax;
            let v = go(r);
            r.all[i] = self.reg;
            return v;
        }
    }
    impl<R: Rng> Backend<R> {
        pub fn new(rng: R) -> Self {
            return Self { rng, all: vec![] };
        }
        pub fn regalloc<T, E: From<IcedError>>(
            &mut self,
            c: &mut CodeAssembler,
            go: impl for<'a> FnOnce(&mut Self, &mut CodeAssembler, &'a mut Token) -> Result<T, E>,
        ) -> Result<T, E> {
            let rs = &[r8, r9, r10, r11, r12, r13, r14, r15];
            let mut r = *rs.choose(&mut self.rng).unwrap();
            while self.all.contains(&r) {
                r = *rs.choose(&mut self.rng).unwrap();
            }
            self.all.push(r);
            c.push(r)?;
            let v = go(self, c, &mut Token { reg: r });
            self.all.pop();
            c.pop(r)?;
            return v;
        }
        pub fn taint<T, E: From<IcedError>>(
            &mut self,
            r: &[AsmRegister64],
            c: &mut CodeAssembler,
            go: impl FnOnce(&mut Self, &mut CodeAssembler) -> Result<T, E>,
        ) -> Result<T, E> {
            self.all.extend_from_slice(r);
            for s in r {
                c.push(*s)?;
            }
            let g = go(self, c);
            for s in r.iter().rev() {
                c.pop(*s)?;
            }
            self.all.truncate(self.all.len() - r.len());
            return g;
        }
    }
}
pub mod stalloc {
    use rand::Rng;

    use super::regalloc::{Backend, Token};
    use iced_x86::code_asm::*;
    #[derive(Default)]
    pub struct Frame {
        size: i32,
    }
    #[derive(Clone)]
    pub struct Var {
        idx: i32,
    }
    impl Frame {
        pub fn alloc(&mut self, size: i32) -> Var {
            let v = Var { idx: self.size };
            self.size += size;
            return v;
        }
    }
    pub struct FuncToken<'a> {
        stack: &'a mut Token,
        frame: Frame,
    }
    impl<'a> FuncToken<'a> {
        pub fn bud<'b>(&'b mut self) -> FuncToken<'b> {
            return FuncToken {
                stack: &mut *self.stack,
                frame: Frame {
                    size: self.frame.size,
                },
            };
        }
        pub fn load(
            &mut self,
            c: &mut CodeAssembler,
            target: AsmRegister64,
            var: Var,
            offset: i32,
        ) -> Result<(), IcedError> {
            c.mov(target, self.stack.reg() + (var.idx + offset))?;
            return Ok(());
        }
        pub fn addrof(
            &mut self,
            c: &mut CodeAssembler,
            target: AsmRegister64,
            var: Var,
            offset: i32,
        ) -> Result<(), IcedError> {
            c.lea(target, self.stack.reg() + (var.idx + offset))?;
            return Ok(());
        }
        pub fn store(
            &mut self,
            c: &mut CodeAssembler,
            target: AsmRegister64,
            var: Var,
            offset: i32,
        ) -> Result<(), IcedError> {
            c.mov(self.stack.reg() + (var.idx + offset), target)?;
            return Ok(());
        }
        pub unsafe fn before_ret(&mut self, c: &mut CodeAssembler) -> Result<(), IcedError> {
            c.mov(rsp, self.stack.reg())?;
            c.add(rsp, self.frame.size)?;
            return Ok(());
        }
        pub fn jmp<R: Rng>(
            &mut self,
            c: &mut CodeAssembler,
            l: AsmMemoryOperand,
            b: &mut Backend<R>,
        ) -> Result<(), IcedError> {
            return b.regalloc(c, |b, c, t| {
                c.lea(t.reg(), l)?;
                unsafe { self.before_ret(c)? };
                c.jmp(t.reg())?;
                return Ok(());
            });
        }
        pub fn ret<R: Rng>(
            &mut self,
            c: &mut CodeAssembler,
            b: &mut Backend<R>,
        ) -> Result<(), IcedError> {
            return b.regalloc(c, |b, c, t| {
                unsafe { self.before_ret(c)? };
                c.ret()?;
                return Ok(());
            });
        }
    }
    pub fn func<R: Rng, T, E: From<IcedError>>(
        b: &mut Backend<R>,
        c: &mut CodeAssembler,
        frame: Frame,
        go: impl for<'a> FnOnce(FuncToken<'a>, &mut Backend<R>, &mut CodeAssembler) -> Result<T, E>,
    ) -> Result<T, E> {
        return b.regalloc(c, move |b, c, token| {
            c.mov(token.reg(), rsp)?;
            c.sub(rsp, frame.size)?;
            let token2 = &mut *token;
            let v = b.regalloc(c, move |b, c, t| {
                c.mov(t.reg(), rsp)?;
                let c2 = &mut *c;
                let t2 = &mut *t;
                let tf = FuncToken { stack: t2, frame };
                let v = token2.unused(b, move |b| go(tf, b, c2));
                c.mov(rsp, t.reg())?;
                return v;
            });
            c.mov(rsp, token.reg())?;
            return v;
        });
    }
}
pub mod exec {
    use std::collections::{BTreeMap, BTreeSet};

    use crate::backend::{Ast, Mod, Plat};
    use rand::Rng;

    use super::{
        mm::MM,
        os,
        regalloc::{Backend, Token},
        stalloc::{func, Frame, FuncToken, Var},
    };
    use iced_x86::code_asm::*;
    impl Mod {
        pub fn x64_render<R: Rng>(
            &self,
            rng: &mut R,
            c: &mut CodeAssembler,
            entry: String,
        ) -> Result<(), IcedError> {
            let mut order = vec![entry];
            for f in self.funcs.keys() {
                if !order.contains(f) {
                    order.push(f.clone());
                }
            }
            let mut syms = BTreeMap::new();
            for o in order.iter() {
                syms.insert(o.clone(), c.create_label());
            }
            for o in order.iter() {
                c.set_label(syms.get_mut(o).unwrap())?;
                let mut backend = Backend::new(&mut *rng);
                let mut go = BTreeSet::new();
                self.funcs.get(o).unwrap().body.vars(&mut go);
                let mut v = BTreeMap::new();
                let mut f = Frame::default();
                for g in go {
                    v.insert(g, f.alloc(8));
                }
                func(&mut backend, &mut *c, f, |mut ft, backend, c| {
                    return backend.regalloc(c, |backend, c, t| {
                        self.funcs.get(o).unwrap().body.x64_render(
                            t,
                            ft.bud(),
                            backend,
                            c,
                            &v,
                            &syms,
                        )?;
                        c.movq(xmm0, t.reg())?;
                        return Ok(());
                    });
                })?;
                c.ret()?;
            }

            return Ok(());
        }
    }
    impl Ast {
        pub fn x64_render<R: Rng>(
            &self,
            target: &mut Token,
            mut fun: FuncToken<'_>,
            backend: &mut Backend<R>,
            c: &mut CodeAssembler,
            vars: &BTreeMap<String, Var>,
            syms: &BTreeMap<String, CodeLabel>,
        ) -> Result<(), IcedError> {
            match self {
                Ast::Call(a, b) => {
                    target.unused(backend, |backend| {
                        for (i, a) in b.iter().enumerate() {
                            backend.regalloc(c, |b, c, t| {
                                a.x64_render(t, fun.bud(), b, c, vars, syms)?;
                                c.movq(AsmRegisterXmm::all()[i], t.reg())?;
                                return Ok(());
                            })?;
                        }
                        return Ok(());
                    })?;
                    a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    c.call(target.reg())?;
                    c.movq(target.reg(), xmm0)?;
                    return Ok(());
                }
                Ast::Sym(s) => {
                    let a = syms.get(s).unwrap();
                    c.lea(target.reg(), a.clone().into())?;
                    return Ok(());
                }
                Ast::Jmp(a, b) => {
                    target.unused(backend, |backend| {
                        for (i, a) in b.iter().enumerate() {
                            backend.regalloc(c, |b, c, t| {
                                a.x64_render(t, fun.bud(), b, c, vars, syms)?;
                                c.movq(AsmRegisterXmm::all()[i], t.reg())?;
                                return Ok(());
                            })?;
                        }
                        return Ok(());
                    })?;
                    a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    fun.jmp(c, target.reg() + 0, backend)?;
                    return Ok(());
                }
                Ast::Var(v) => {
                    let v = vars.get(v).unwrap().clone();
                    fun.load(c, target.reg(), v, 0)?;
                    return Ok(());
                }
                Ast::Assign(v, a) => {
                    a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    let v = vars.get(v).unwrap().clone();
                    fun.store(c, target.reg(), v, 0)?;
                    return Ok(());
                }
                Ast::Alloca(a) => {
                    a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    c.mov(rax, rsp)?;
                    c.sub(rsp, target.reg())?;
                    c.mov(target.reg(), rax)?;
                    return Ok(());
                }
                Ast::Load(a, s) => {
                    a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    let i = [r8, r9, r10, r11, r12, r13, r14, r15].iter().enumerate().find(|a|*a.1 == target.reg()).unwrap().0;
                    match s {
                        crate::backend::Size::_8 => c.mov(
                            [r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b][i],
                            target.reg() + 0,
                        )?,
                        crate::backend::Size::_16 => c.mov(
                            [r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w][i],
                            target.reg() + 0,
                        )?,
                        crate::backend::Size::_32 => c.mov(
                            [r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d][i],
                            target.reg() + 0,
                        )?,
                        crate::backend::Size::_64 => c.mov(target.reg(), target.reg() + 0)?,
                    };
                    return Ok(());
                }
                Ast::Store(a, b,s) => {
                    backend.regalloc(c, |backend, c, t| {
                        target.unused(backend, |backend| {
                            a.x64_render(t, fun.bud(), backend, c, vars, syms)
                        })?;
                        b.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                        let i = [r8, r9, r10, r11, r12, r13, r14, r15].iter().enumerate().find(|a|*a.1 == t.reg()).unwrap().0;
                        match s {
                            crate::backend::Size::_8 => c.mov(
                                target.reg() + 0,
                                [r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b][i],
                            )?,
                            crate::backend::Size::_16 => c.mov(
                                target.reg() + 0,
                                [r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w][i],
                            )?,
                            crate::backend::Size::_32 => c.mov(
                                target.reg() + 0,
                                [r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d][i],
                            )?,
                            crate::backend::Size::_64 => c.mov(target.reg() + 0,t.reg())?,
                        };
                        // c.mov(target.reg() + 0, t.reg())?;
                        return Ok(());
                    })?;
                    return Ok(());
                }
                Ast::Const(co) => {
                    c.mov(target.reg(), *co)?;
                    return Ok(());
                }
                Ast::Bin(o, a, b) => {
                    backend.regalloc(c, |backend, c, t| {
                        target.unused(backend, |backend| {
                            b.x64_render(t, fun.bud(), backend, c, vars, syms)
                        })?;
                        a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                        match o {
                            crate::backend::BinOp::Add => c.add(target.reg(), t.reg())?,
                            crate::backend::BinOp::Sub => c.sub(target.reg(), t.reg())?,
                            crate::backend::BinOp::Mul => c.imul_2(target.reg(), t.reg())?,
                            crate::backend::BinOp::And => c.and(target.reg(), t.reg())?,
                            crate::backend::BinOp::Or => c.or(target.reg(), t.reg())?,
                            crate::backend::BinOp::Xor => c.xor(target.reg(), t.reg())?,
                            crate::backend::BinOp::DivU => {
                                c.mov(rax, target.reg())?;
                                c.div(t.reg())?;
                                c.mov(target.reg(), rax)?;
                            }
                            crate::backend::BinOp::ModU => {
                                c.mov(rax, target.reg())?;
                                c.div(t.reg())?;
                                c.mov(target.reg(), rdx)?;
                            }
                            crate::backend::BinOp::DivS => {
                                c.mov(rax, target.reg())?;
                                c.cqo()?;
                                c.idiv(t.reg())?;
                                c.mov(target.reg(), rax)?;
                            }
                            crate::backend::BinOp::ModS => {
                                c.mov(rax, target.reg())?;
                                c.cqo()?;
                                c.idiv(t.reg())?;
                                c.mov(target.reg(), rdx)?;
                            }
                            crate::backend::BinOp::Shl => {
                                c.mov(rax, t.reg())?;
                                c.shl(target.reg(), al)?;
                            }
                            crate::backend::BinOp::ShrS => {
                                c.mov(rax, t.reg())?;
                                c.sar(target.reg(), al)?;
                            }
                            crate::backend::BinOp::ShrU => {
                                c.mov(rax, t.reg())?;
                                c.shr(target.reg(), al)?;
                            }
                        }
                        return Ok(());
                    })?;
                    return Ok(());
                }
                Ast::If {
                    cond,
                    if_true,
                    if_false,
                    cty,
                } => {
                    cond.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    let mut l = c.create_label();
                    let mut f = c.create_label();
                    c.lea(rax, l.into())?;
                    c.lea(rbx, f.into())?;
                    c.cmp(target.reg(), 0)?;
                    match cty {
                        crate::backend::CondType::Eq => c.cmovne(rax, rbx)?,
                        crate::backend::CondType::Greater => c.cmovle(rax, rbx)?,
                    };
                    c.jmp(rax)?;
                    let mut end = c.create_label();
                    c.set_label(&mut f)?;
                    if_true.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    c.lea(rax, end.into())?;
                    c.jmp(rax)?;
                    c.set_label(&mut l)?;
                    if_false.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    c.set_label(&mut end)?;
                    return Ok(());
                }
                Ast::Param(p) => {
                    c.movq(target.reg(), AsmRegisterXmm::all()[*p])?;
                    return Ok(());
                }
                Ast::Plat(p, l) => match p {
                    Plat::Linux => {
                        backend.taint(&[r10, r8, r9], c, |backend, c| {
                            return backend.regalloc(c, |backend, c, target| {
                                for (a, b) in
                                    l.iter().zip((&[rax, rdi, rsi, rdx, r10, r8, r9]).iter())
                                {
                                    a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                                    c.mov(*b, target.reg())?;
                                }
                                c.syscall()?;
                                return Ok(());
                            });
                        })?;
                        c.mov(target.reg(), rax)?;
                        return Ok(());
                    }
                    _ => {
                        c.ud2()?;
                        return Ok(());
                    }
                },
                Ast::OS => {
                    os::detect(c)?;
                    c.mov(target.reg(), rax)?;
                    return Ok(());
                }
                Ast::Ret(a) => {
                    a.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    c.movq(AsmRegisterXmm::all()[0], target.reg())?;
                    fun.ret(c, backend)?;
                    return Ok(());
                }
                Ast::DoWhile { body, cond, cty } => {
                    return backend.regalloc(c, |backend, c, t| {
                        let mut again = c.create_label();
                        let mut l = c.create_label();
                        c.set_label(&mut again)?;
                        body.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                        cond.x64_render(t, fun.bud(), backend, c, vars, syms)?;
                        c.lea(rax, l.into())?;
                        c.lea(rbx, again.into())?;
                        c.cmp(target.reg(), 0)?;
                        match cty {
                            crate::backend::CondType::Eq => c.cmovne(rax, rbx)?,
                            crate::backend::CondType::Greater => c.cmovle(rax, rbx)?,
                        };
                        c.jmp(rax)?;
                        c.set_label(&mut l)?;
                        return Ok(());
                    });
                }
                Ast::Many(m) => {
                    for m in m.iter() {
                        m.x64_render(target, fun.bud(), backend, c, vars, syms)?;
                    }
                    return Ok(());
                }
                Ast::Data(d) => {
                    let mut l = c.create_label();
                    c.lea(target.reg(),l.into())?;
                    c.call(target.reg())?;
                    c.db(d.as_ref())?;
                    c.set_label(&mut l)?;
                    c.pop(target.reg())?;
                    return Ok(());
                },
            }
        }
    }
}
pub mod os {
    use iced_x86::code_asm::*;

    pub const OS_W64: u32 = 0;
    pub const OS_L64: u32 = 1;
    pub const OS_BSD64: u32 = 2;
    pub const OS_WASM: u32 = 0xff;
    pub fn detect(c: &mut CodeAssembler) -> Result<(), IcedError> {
        c.push(r10)?;
        c.push(r8)?;
        let mut ex_ga = c.create_label();
        c.lea(r10, ex_ga.into())?;
        c.push(6)?;
        c.pop(eax)?;
        c.cdq()?;
        c.push(edx)?;
        c.pop(edi)?;
        c.syscall()?;
        c.xor(edx, edx)?;
        c.cmp(eax, 0xc0000005u32)?;
        let mut next = c.create_label();
        c.lea(r8, next.into())?;
        c.cmovz(r8, r10)?;
        c.jmp(r8)?;
        c.set_label(&mut next)?;
        c.inc(dl)?;
        c.test(eax, eax)?;
        let mut next = c.create_label();
        c.lea(r8, next.into())?;
        c.cmovl(r8, r10)?;
        c.jmp(r8)?;
        c.set_label(&mut next)?;
        c.mov(dl, OS_BSD64)?;
        c.set_label(&mut ex_ga)?;
        c.xchg(eax, edx)?;
        c.pop(r8)?;
        c.pop(r10)?;

        return Ok(());
    }
}
