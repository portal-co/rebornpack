use proc_macro2::{Span, TokenStream};
use rat_ast::export::rust::RustOp;
use rat_ir::{bi::license::Taint, no_push};
use syn::Ident;
use waffle::{MemoryArg, Operator, SignatureData};
use crate::OpWrapper;
impl RustOp for OpWrapper{
    fn rust(&self, mut args: impl Iterator<Item = proc_macro2::TokenStream>) -> proc_macro2::TokenStream {
        use quote::quote;
        macro_rules! binop {
            ($bl:tt,$vn:tt, $ra:tt, $rb:tt, $args:tt, $op:tt) => {{
                let $ra = $args[0].clone();
                //let $ra = Ident::new(&ra.to_string(), Span::call_site());
                let $rb = $args[1].clone();
                //let $rb = Ident::new(&rb.to_string(), Span::call_site());
                $bl = quote! {
                    #$bl;
                    let #$vn = $op;
                }
            }};
        }
        macro_rules! ternop {
            ($bl:tt,$vn:tt, $ra:tt, $rb:tt,$rc:tt, $args:tt, $op:tt) => {{
                let $ra = $args[0].clone();
                //let $ra = Ident::new(&ra.to_string(), Span::call_site());
                let $rb = $args[1].clone();
                //let $rb = Ident::new(&rb.to_string(), Span::call_site());
                let $rc = $args[2].clone();
                //let $rc = Ident::new(&rb.to_string(), Span::call_site());
                $bl = quote! {
                    #$bl;
                    let #$vn = $op;
                }
            }};
        }
        macro_rules! unop {
            ($bl:tt,$vn:tt, $ra:tt, $args:tt, $op:tt) => {{
                let $ra = $args[0].clone();
                //let $ra = Ident::new(&ra.to_string(), Span::call_site());
                $bl = quote! {
                    #$bl;
                    let #$vn = $op;
                }
            }};
        }
        let ctx = args.next().unwrap();
        let args: Vec<_> = args.collect();
        pub fn memory_(m: &MemoryArg) -> proc_macro2::TokenStream {
            let n = Ident::new(&m.memory.to_string(), Span::call_site());
            let x = m.offset as usize;
            return quote! {
                (&mut (ctx.#n())[#x..])
            };
        }
        pub fn memory_arg(m: &MemoryArg, args: &[TokenStream]) -> proc_macro2::TokenStream {
            let m = memory_(m);
            let o = args[0].clone();
            // let o = Ident::new(&o.to_string(), Span::call_site());
            let o = quote! {
                (&mut (#m)[(#o as usize)..])
            };
            return o;
        }
        let vn = Ident::new("target", Span::call_site());
        let mut bl = quote!{
            let mut ctx = #ctx.lock().unwrap();
        };
        match &self.0 {
            waffle::Operator::Unreachable => {
            }
            waffle::Operator::Nop => {
            }
            waffle::Operator::RefIsNull => {
                unop!(bl,vn,ra,args,({match #ra.clone(){None => 1, Some(_) => 0}}))
            }
            waffle::Operator::RefNull { .. } => {
                bl = quote! {
                    #bl;
                    let #vn = None;
                }
            }
            // waffle::Operator::RefFunc { func_index } => {
            //     let function_index = func_index;
            //     let mut a = Block::invalid();
            //     if let Some(d) = m.funcs[function_index].body() {
            //         a = d.entry;
            //     }
            //     let state =
            //         Ident::new(&format!("{ident}{function_index}State"), ident.span());
            //     let mi = mangle(m, function_index, a);
            //     let ty = b.type_pool[rts][0].clone();
            //     let ty = render_type(m, &ty, opts, quote!(ctx));
            //     bl = quote! {
            //         #bl;
            //         let #vn: #ty = Some(Arc::new(ctx.#mi));
            //     }
            // }
            // waffle::Operator::Call { function_index } => {
            //     let mut a = Block::invalid();
            //     if let Some(d) = m.funcs[function_index].body() {
            //         a = d.entry;
            //     }
            //     let state =
            //         Ident::new(&format!("{ident}{function_index}State"), ident.span());
            //     let mi = mangle(m, function_index, a);
            //     let args = args
            //         .iter()
            //         .map(|i| Ident::new(&i.to_string(), Span::call_site()))
            //         .collect::<Vec<_>>();
            //     let r = m.funcs[function_index].sig();
            //     let r = m.signatures[r].clone();
            //     let mut args = quote! {
            //         #(#args .clone()),*
            //     };
            //     if opts.needs_tco() {
            //         match m.funcs[function_index].body() {
            //             None => {
            //                 args = quote! {
            //                     #state::Of(#args)
            //                 }
            //             }
            //             Some(b) => {
            //                 let b = b.entry;
            //                 let b = Ident::new(&b.to_string(), Span::call_site());
            //                 args = quote! {
            //                     #state::#b(#args)
            //                 }
            //             }
            //         }
            //     };

            //     bl = quote! {
            //         #bl;
            //         let #vn = ctx.#mi(#args)
            //     };
            //     if opts.r#async {
            //         bl = quote! {
            //             #bl;
            //             let #vn = Box::pin(#vn).await;
            //         };
            //     }
            //     if opts.result {
            //         bl = quote! {
            //             #bl;
            //             let #vn = #vn?;
            //         };
            //     }
            //     if opts.r#impl {
            //         bl = quote! {
            //             #bl;
            //             let (#vn,this) = #vn;
            //         }
            //     }
            //     // bl = quote!{
            //     //     #bl;
            //     //     let #vn = #vn #wrap;
            //     // };
            //     if r.returns.len() == 1 {
            //         bl = quote! {
            //             #bl;
            //             let #vn = #vn.0
            //         }
            //     }
            // }
            // waffle::Operator::CallRef { sig_index } => {
            //     let mut args = args
            //         .iter()
            //         .map(|i| Ident::new(&i.to_string(), Span::call_site()))
            //         .map(|a| quote!(#a))
            //         .collect::<Vec<_>>();
            //     let mi = args.pop().unwrap();
            //     args = vec![quote!(&mut *ctx)]
            //         .into_iter()
            //         .chain(args.into_iter())
            //         .collect();
            //     // let table = Ident::new(&table_index.to_string(),Span::call_site());
            //     // let mi = quote!{
            //     //     ctx.z().#table[#mi as usize].clone()
            //     // };
            //     // let r = m.funcs[function_index].sig();
            //     // let r = m.signatures[r].clone();
            //     let mut args = quote! {
            //         #(#args .clone()),*
            //     };
            //     // if opts.needs_tco() {
            //     //     match m.funcs[function_index].body() {
            //     //         None => {
            //     //             args = quote! {
            //     //                 #state::Of(#args)
            //     //             }
            //     //         }
            //     //         Some(b) => {
            //     //             let b = b.entry;
            //     //             let b = Ident::new(&b.to_string(), Span::call_site());
            //     //             args = quote! {
            //     //                 #state::#b(#args)
            //     //             }
            //     //         }
            //     //     }
            //     // };

            //     bl = quote! {
            //         #bl;
            //         let #vn = (#mi.unwrap())(#args)
            //     };
            //     if opts.r#async {
            //         bl = quote! {
            //             #bl;
            //             let #vn = Box::pin(#vn).await;
            //         };
            //     }
            //     if opts.result {
            //         bl = quote! {
            //             #bl;
            //             let #vn = #vn?;
            //         };
            //     }
            //     if opts.r#impl {
            //         bl = quote! {
            //             #bl;
            //             let (#vn,this) = #vn;
            //         }
            //     }
            //     // bl = quote!{
            //     //     #bl;
            //     //     let #vn = #vn #wrap;
            //     // };
            //     if b.type_pool[rts].len() == 1 {
            //         bl = quote! {
            //             #bl;
            //             let #vn = #vn.0
            //         }
            //     }
            // }
            // waffle::Operator::CallIndirect {
            //     sig_index,
            //     table_index,
            // } => {
            //     // let mut a = Block::invalid();
            //     // if let Some(d) = m.funcs[function_index].body() {
            //     //     a = d.entry;
            //     // }
            //     // let state =
            //     //     Ident::new(&format!("{ident}{function_index}State"), ident.span());
            //     // let mi = mangle(m, function_index, a);
            //     let mut args = args
            //         .iter()
            //         .map(|i| Ident::new(&i.to_string(), Span::call_site()))
            //         .map(|a| quote!(#a))
            //         .collect::<Vec<_>>();
            //     let mi = args[0].clone();
            //     args[0] = quote!(&mut *ctx);
            //     let table = Ident::new(&table_index.to_string(), Span::call_site());
            //     let mi = quote! {
            //         (ctx.z().#table[#mi as usize].clone().unwrap())
            //     };
            //     // let r = m.funcs[function_index].sig();
            //     // let r = m.signatures[r].clone();
            //     let mut args = quote! {
            //         #(#args .clone()),*
            //     };
            //     // if opts.needs_tco() {
            //     //     match m.funcs[function_index].body() {
            //     //         None => {
            //     //             args = quote! {
            //     //                 #state::Of(#args)
            //     //             }
            //     //         }
            //     //         Some(b) => {
            //     //             let b = b.entry;
            //     //             let b = Ident::new(&b.to_string(), Span::call_site());
            //     //             args = quote! {
            //     //                 #state::#b(#args)
            //     //             }
            //     //         }
            //     //     }
            //     // };

            //     bl = quote! {
            //         #bl;
            //         let #vn = #mi(#args)
            //     };
            //     if opts.r#async {
            //         bl = quote! {
            //             #bl;
            //             let #vn = Box::pin(#vn).await;
            //         };
            //     }
            //     if opts.result {
            //         bl = quote! {
            //             #bl;
            //             let #vn = #vn?;
            //         };
            //     }
            //     if opts.r#impl {
            //         bl = quote! {
            //             #bl;
            //             let (#vn,this) = #vn;
            //         }
            //     }
            //     // bl = quote!{
            //     //     #bl;
            //     //     let #vn = #vn #wrap;
            //     // };
            //     if b.type_pool[rts].len() == 1 {
            //         bl = quote! {
            //             #bl;
            //             let #vn = #vn.0
            //         }
            //     }
            // }
            waffle::Operator::Select => {
                ternop!(bl,vn,ra,rb,rc,args,(if #rc != 0{#ra.clone()}else{#rb.clone()}))
            }
            waffle::Operator::TypedSelect { ty } => {
                ternop!(bl,vn,ra,rb,rc,args,(if #rc != 0{#ra.clone()}else{#rb.clone()}))
            }
            waffle::Operator::GlobalGet { global_index } => {
                let g = Ident::new(&global_index.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    let #vn = *ctx.#g()
                }
            }
            waffle::Operator::GlobalSet { global_index } => {
                let g = Ident::new(&global_index.to_string(), Span::call_site());
                let r = Ident::new(&args[0].to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    *ctx.#g() = #r
                }
            }
            waffle::Operator::I32Load { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = u32::from_le_bytes((&#o[0..4]).try_into().unwrap())
                };
            }
            waffle::Operator::I64Load { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = u64::from_le_bytes((&#o[0..8]).try_into().unwrap())
                };
            }
            waffle::Operator::F32Load { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = unsafe{std::mem::transmute::<_,f32>(u32::from_le_bytes((&#o[0..4]).try_into().unwrap()))}
                };
            }
            waffle::Operator::F64Load { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = unsafe{std::mem::transmute::<_,f64>(u64::from_le_bytes((&#o[0..8]).try_into().unwrap()))}
                };
            }
            waffle::Operator::I32Load8S { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = i8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i32;
                    let #vn = unsafe{
                        std::mem::transmute::<_,u32>(#vn)
                    }
                };
            }
            waffle::Operator::I32Load8U { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = u8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u32
                };
            }
            waffle::Operator::I32Load16S { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = i16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i32;
                    let #vn = unsafe{
                        std::mem::transmute::<_,u32>(#vn)
                    }
                };
            }
            waffle::Operator::I32Load16U { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = u16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u32
                };
            }
            waffle::Operator::I64Load8S { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = i8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i64;
                    let #vn = unsafe{
                        std::mem::transmute::<_,u64>(#vn)
                    }
                };
            }
            waffle::Operator::I64Load8U { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = u8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u64
                };
            }
            waffle::Operator::I64Load16S { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = i16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i64;
                    let #vn = unsafe{
                        std::mem::transmute::<_,u64>(#vn)
                    }
                };
            }
            waffle::Operator::I64Load16U { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = u16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u64
                };
            }
            waffle::Operator::I64Load32S { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = i32::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i64;
                    let #vn = unsafe{
                        std::mem::transmute::<_,u64>(#vn)
                    }
                };
            }
            waffle::Operator::I64Load32U { memory } => {
                let o = memory_arg(memory, &args);
                bl = quote! {
                    #bl;
                    let #vn = u32::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u64
                };
            }
            waffle::Operator::I32Store { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..4]).copy_from_slice(&(u32::to_le_bytes(#n)));
                    let #vn = 0
                };
            }
            waffle::Operator::I64Store { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..8]).copy_from_slice(&(u64::to_le_bytes(#n)));
                    let #vn = 0
                };
            }
            waffle::Operator::F32Store { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..4]).copy_from_slice(&(u32::to_le_bytes(unsafe{
                        std::mem::transmute::<f32,u32>(#n)
                    })));
                    let #vn = 0
                };
            }
            waffle::Operator::F64Store { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..8]).copy_from_slice(&(u64::to_le_bytes(unsafe{
                        std::mem::transmute::<f64,u64>(#n)
                    })));
                    let #vn = 0
                };
            }
            waffle::Operator::I32Store8 { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..1]).copy_from_slice(&(u8::to_le_bytes((#n & 0xff) as u8)));
                    let #vn = 0
                };
            }
            waffle::Operator::I32Store16 { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..2]).copy_from_slice(&(u16::to_le_bytes((#n & 0xffff) as u16)));
                    let #vn = 0
                };
            }
            waffle::Operator::I64Store8 { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..1]).copy_from_slice(&(u8::to_le_bytes((#n & 0xff) as u8)));
                    let #vn = 0
                };
            }
            waffle::Operator::I64Store16 { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..2]).copy_from_slice(&(u16::to_le_bytes((#n & 0xffff) as u16)));
                    let #vn = 0
                };
            }
            waffle::Operator::I64Store32 { memory } => {
                let o = memory_arg(memory, &args);
                let n = args[1].clone();
                let n = Ident::new(&n.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    (&mut #o[0..4]).copy_from_slice(&(u32::to_le_bytes((#n & 0xffffffff) as u32)));
                    let #vn = 0
                };
            }
            waffle::Operator::I32Const { value } => {
                bl = quote! {
                    #bl;
                    let #vn : u32 = #value;
                }
            }
            waffle::Operator::I64Const { value } => {
                bl = quote! {
                    #bl;
                    let #vn : u64 = #value;
                }
            }
            waffle::Operator::F32Const { value } => {
                bl = quote! {
                    #bl;
                    let #vn : f32 = unsafe{std::mem::transmute(#value)};
                }
            }
            waffle::Operator::F64Const { value } => {
                bl = quote! {
                    #bl;
                    let #vn : f64 = unsafe{std::mem::transmute(#value)};
                }
            }
            waffle::Operator::I32Eqz => unop!(bl,vn,ra,args,((#ra == 0) as u32)),
            waffle::Operator::I32Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
            waffle::Operator::I32Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
            waffle::Operator::I32LtS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i32>(#ra) < std::mem::transmute::<_,i32>(#rb)) as u32)
            })),
            waffle::Operator::I32LtU => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
            waffle::Operator::I32GtS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i32>(#ra) > std::mem::transmute::<_,i32>(#rb)) as u32)
            })),
            waffle::Operator::I32GtU => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
            waffle::Operator::I32LeS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i32>(#ra) <= std::mem::transmute::<_,i32>(#rb)) as u32)
            })),
            waffle::Operator::I32LeU => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
            waffle::Operator::I32GeS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i32>(#ra) >= std::mem::transmute::<_,i32>(#rb)) as u32)
            })),
            waffle::Operator::I32GeU => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),
            waffle::Operator::I64Eqz => unop!(bl,vn,ra,args,((#ra == 0) as u32)),
            waffle::Operator::I64Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
            waffle::Operator::I64Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
            waffle::Operator::I64LtS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i64>(#ra) < std::mem::transmute::<_,i64>(#rb)) as u32)
            })),
            waffle::Operator::I64LtU => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
            waffle::Operator::I64GtS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i64>(#ra) > std::mem::transmute::<_,i64>(#rb)) as u32)
            })),
            waffle::Operator::I64GtU => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
            waffle::Operator::I64LeS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i64>(#ra) <= std::mem::transmute::<_,i64>(#rb)) as u32)
            })),
            waffle::Operator::I64LeU => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
            waffle::Operator::I64GeS => binop!(bl,vn,ra,rb,args,(unsafe{
                ((std::mem::transmute::<_,i64>(#ra) >= std::mem::transmute::<_,i64>(#rb)) as u32)
            })),
            waffle::Operator::I64GeU => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),
            waffle::Operator::F32Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
            waffle::Operator::F32Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
            waffle::Operator::F32Lt => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
            waffle::Operator::F32Gt => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
            waffle::Operator::F32Le => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
            waffle::Operator::F32Ge => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),

            waffle::Operator::F64Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
            waffle::Operator::F64Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
            waffle::Operator::F64Lt => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
            waffle::Operator::F64Gt => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
            waffle::Operator::F64Le => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
            waffle::Operator::F64Ge => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),

            waffle::Operator::I32Clz => unop!(bl,vn,ra,args,(#ra.leading_zeros())),
            waffle::Operator::I32Ctz => unop!(bl,vn,ra,args,(#ra.trailing_zeros())),
            waffle::Operator::I32Popcnt => unop!(bl,vn,ra,args,(#ra.count_ones())),
            waffle::Operator::I32Add => binop!(bl,vn,ra,rb,args,(#ra.wrapping_add(#rb))),
            waffle::Operator::I32Sub => binop!(bl,vn,ra,rb,args,(#ra.wrapping_sub(#rb))),
            waffle::Operator::I32Mul => binop!(bl,vn,ra,rb,args,(#ra.wrapping_mul(#rb))),
            waffle::Operator::I32DivS => binop!(bl,vn,ra,rb,args,(unsafe{
                std::mem::transmute::<_,u32>((std::mem::transmute::<_,i32>(#ra) / std::mem::transmute::<_,i32>(#rb)))
            })),
            waffle::Operator::I32DivU => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
            waffle::Operator::I32RemS => binop!(bl,vn,ra,rb,args,(unsafe{
                std::mem::transmute::<_,u32>((std::mem::transmute::<_,i32>(#ra) % std::mem::transmute::<_,i32>(#rb)))
            })),
            waffle::Operator::I32RemU => binop!(bl,vn,ra,rb,args,(#ra % #rb)),
            waffle::Operator::I32And => binop!(bl,vn,ra,rb,args,(#ra & #rb)),
            waffle::Operator::I32Or => binop!(bl,vn,ra,rb,args,(#ra | #rb)),
            waffle::Operator::I32Xor => binop!(bl,vn,ra,rb,args,(#ra ^ #rb)),
            waffle::Operator::I32Shl => binop!(bl,vn,ra,rb,args,(#ra << #rb)),
            waffle::Operator::I32ShrS => binop!(bl,vn,ra,rb,args,(unsafe{
                std::mem::transmute::<_,u32>((std::mem::transmute::<_,i32>(#ra) >> std::mem::transmute::<_,i32>(#rb)))
            })),
            waffle::Operator::I32ShrU => binop!(bl,vn,ra,rb,args,(#ra >> #rb)),
            waffle::Operator::I32Rotl => binop!(bl,vn,ra,rb,args,(#ra.rotate_left(#rb))),
            waffle::Operator::I32Rotr => binop!(bl,vn,ra,rb,args,(#ra.rotate_right(#rb))),
            waffle::Operator::I64Clz => unop!(bl,vn,ra,args,(#ra.leading_zeros() as u64)),
            waffle::Operator::I64Ctz => unop!(bl,vn,ra,args,(#ra.trailing_zeros() as u64)),
            waffle::Operator::I64Popcnt => unop!(bl,vn,ra,args,(#ra.count_ones() as u64)),
            waffle::Operator::I64Add => binop!(bl,vn,ra,rb,args,(#ra.wrapping_add(#rb))),
            waffle::Operator::I64Sub => binop!(bl,vn,ra,rb,args,(#ra.wrapping_sub(#rb))),
            waffle::Operator::I64Mul => binop!(bl,vn,ra,rb,args,(#ra.wrapping_mul(#rb))),
            waffle::Operator::I64DivS => binop!(bl,vn,ra,rb,args,(unsafe{
                std::mem::transmute::<_,u64>((std::mem::transmute::<_,i64>(#ra) / std::mem::transmute::<_,i64>(#rb)))
            })),
            waffle::Operator::I64DivU => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
            waffle::Operator::I64RemS => binop!(bl,vn,ra,rb,args,(unsafe{
                std::mem::transmute::<_,u64>((std::mem::transmute::<_,i64>(#ra) % std::mem::transmute::<_,i64>(#rb)))
            })),
            waffle::Operator::I64RemU => binop!(bl,vn,ra,rb,args,(#ra % #rb)),
            waffle::Operator::I64And => binop!(bl,vn,ra,rb,args,(#ra & #rb)),
            waffle::Operator::I64Or => binop!(bl,vn,ra,rb,args,(#ra | #rb)),
            waffle::Operator::I64Xor => binop!(bl,vn,ra,rb,args,(#ra ^ #rb)),
            waffle::Operator::I64Shl => binop!(bl,vn,ra,rb,args,(#ra << #rb)),
            waffle::Operator::I64ShrS => binop!(bl,vn,ra,rb,args,(unsafe{
                std::mem::transmute::<_,u64>((std::mem::transmute::<_,i64>(#ra) >> std::mem::transmute::<_,i64>(#rb)))
            })),
            waffle::Operator::I64ShrU => binop!(bl,vn,ra,rb,args,(#ra >> #rb)),
            waffle::Operator::I64Rotl => {
                binop!(bl,vn,ra,rb,args,(#ra.rotate_left(#rb.try_into().unwrap())))
            }
            waffle::Operator::I64Rotr => {
                binop!(bl,vn,ra,rb,args,(#ra.rotate_right(#rb.try_into().unwrap())))
            }
            waffle::Operator::F32Abs => unop!(bl,vn,ra,args,((#ra).abs())),
            waffle::Operator::F32Neg => unop!(bl,vn,ra,args,(0f32-#ra)),
            waffle::Operator::F32Ceil => unop!(bl,vn,ra,args,(#ra.ceil())),
            waffle::Operator::F32Floor => unop!(bl,vn,ra,args,(#ra.floor())),
            waffle::Operator::F32Trunc => unop!(bl,vn,ra,args,(#ra.trunc())),
            waffle::Operator::F32Nearest => unop!(bl,vn,ra,args,(#ra.round())),
            waffle::Operator::F32Sqrt => unop!(bl,vn,ra,args,(#ra.sqrt())),
            waffle::Operator::F32Add => binop!(bl,vn,ra,rb,args,(#ra + #rb)),
            waffle::Operator::F32Sub => binop!(bl,vn,ra,rb,args,(#ra - #rb)),
            waffle::Operator::F32Mul => binop!(bl,vn,ra,rb,args,(#ra * #rb)),
            waffle::Operator::F32Div => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
            waffle::Operator::F32Min => binop!(bl,vn,ra,rb,args,(#ra.min(#rb))),
            waffle::Operator::F32Max => binop!(bl,vn,ra,rb,args,(#ra.max(#rb))),
            waffle::Operator::F32Copysign => {
                binop!(bl,vn,ra,rb,args,(#rb.signum() * #ra.abs()))
            }

            waffle::Operator::F64Abs => unop!(bl,vn,ra,args,((#ra).abs())),
            waffle::Operator::F64Neg => unop!(bl,vn,ra,args,(0f64-#ra)),
            waffle::Operator::F64Ceil => unop!(bl,vn,ra,args,(#ra.ceil())),
            waffle::Operator::F64Floor => unop!(bl,vn,ra,args,(#ra.floor())),
            waffle::Operator::F64Trunc => unop!(bl,vn,ra,args,(#ra.trunc())),
            waffle::Operator::F64Nearest => unop!(bl,vn,ra,args,(#ra.round())),
            waffle::Operator::F64Sqrt => unop!(bl,vn,ra,args,(#ra.sqrt())),
            waffle::Operator::F64Add => binop!(bl,vn,ra,rb,args,(#ra + #rb)),
            waffle::Operator::F64Sub => binop!(bl,vn,ra,rb,args,(#ra - #rb)),
            waffle::Operator::F64Mul => binop!(bl,vn,ra,rb,args,(#ra * #rb)),
            waffle::Operator::F64Div => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
            waffle::Operator::F64Min => binop!(bl,vn,ra,rb,args,(#ra.min(#rb))),
            waffle::Operator::F64Max => binop!(bl,vn,ra,rb,args,(#ra.max(#rb))),
            waffle::Operator::F64Copysign => {
                binop!(bl,vn,ra,rb,args,(#rb.signum() * #ra.abs()))
            }

            waffle::Operator::I32WrapI64 => unop!(bl,vn,ra,args,((#ra & 0xfffffff) as u32)),
            waffle::Operator::I32TruncF32S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc() as i32)}))
            }
            waffle::Operator::I32TruncF32U => unop!(bl,vn,ra,args,(#ra.trunc() as u32)),
            waffle::Operator::I32TruncF64S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc() as i32)}))
            }
            waffle::Operator::I32TruncF64U => unop!(bl,vn,ra,args,(#ra.trunc() as u32)),
            waffle::Operator::I64TruncF32S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u64>(#ra.trunc() as i64)}))
            }
            waffle::Operator::I64TruncF32U => unop!(bl,vn,ra,args,(#ra.trunc() as u64)),
            waffle::Operator::I64TruncF64S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u64>(#ra.trunc() as i64)}))
            }
            waffle::Operator::I64TruncF64U => unop!(bl,vn,ra,args,(#ra.trunc() as u64)),
            waffle::Operator::I64ExtendI32S => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u64>(std::mem::transmute::<u32,i32>(#ra) as i64)
            })),
            waffle::Operator::I64ExtendI32U => unop!(bl,vn,ra,args,(#ra as u64)),
            waffle::Operator::F32ConvertI32S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i32>(#ra) as f32}))
            }
            waffle::Operator::F32ConvertI32U => unop!(bl,vn,ra,args,(unsafe{#ra as f32})),
            waffle::Operator::F32ConvertI64S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i64>(#ra) as f32}))
            }
            waffle::Operator::F32ConvertI64U => unop!(bl,vn,ra,args,(unsafe{#ra as f32})),
            waffle::Operator::F32DemoteF64 => unop!(bl,vn,ra,args,(#ra as f32)),

            waffle::Operator::F64ConvertI32S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i32>(#ra) as f64}))
            }
            waffle::Operator::F64ConvertI32U => unop!(bl,vn,ra,args,(unsafe{#ra as f64})),
            waffle::Operator::F64ConvertI64S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i64>(#ra) as f64}))
            }
            waffle::Operator::F64ConvertI64U => unop!(bl,vn,ra,args,(unsafe{#ra as f64})),

            waffle::Operator::F64PromoteF32 => unop!(bl,vn,ra,args,(#ra as f64)),
            waffle::Operator::I32Extend8S => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u32>(std::mem::transmute::<u8,i8>((#ra & 0xff) as u8) as i32)
            })),
            waffle::Operator::I32Extend16S => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u32>(std::mem::transmute::<u16,i16>((#ra & 0xffff) as u16) as i32)
            })),
            waffle::Operator::I64Extend8S => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u64>(std::mem::transmute::<u8,i8>((#ra & 0xff) as u8) as i64)
            })),
            waffle::Operator::I64Extend16S => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u64>(std::mem::transmute::<u16,i16>((#ra & 0xffff) as u16) as i64)
            })),
            waffle::Operator::I64Extend32S => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u64>(std::mem::transmute::<u32,i32>((#ra & 0xffffffff) as u32) as i64)
            })),
            // waffle::Operator::I32TruncSatF32S => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            // waffle::Operator::I32TruncSatF32U => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            // waffle::Operator::I32TruncSatF64S => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            // waffle::Operator::I32TruncSatF64U => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            // waffle::Operator::I64TruncSatF32S => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            // waffle::Operator::I64TruncSatF32U => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            // waffle::Operator::I64TruncSatF64S => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            // waffle::Operator::I64TruncSatF64U => {
            //     panic!("not yet implemented: {}; {}", file!(), line!())
            // }
            waffle::Operator::I32TruncSatF32S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i32>())}))
            }
            waffle::Operator::I32TruncSatF32U => {
                unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u32>()))
            }
            waffle::Operator::I32TruncSatF64S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i32>())}))
            }
            waffle::Operator::I32TruncSatF64U => {
                unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u32>()))
            }
            waffle::Operator::I64TruncSatF32S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i64>())}))
            }
            waffle::Operator::I64TruncSatF32U => {
                unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u64>()))
            }
            waffle::Operator::I64TruncSatF64S => {
                unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i64>())}))
            }
            waffle::Operator::I64TruncSatF64U => {
                unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u64>))
            }
            waffle::Operator::F32ReinterpretI32 => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,f32>(#ra)
            })),
            waffle::Operator::F64ReinterpretI64 => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,f64>(#ra)
            })),
            waffle::Operator::I32ReinterpretF32 => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u32>(#ra)
            })),
            waffle::Operator::I64ReinterpretF64 => unop!(bl,vn,ra,args,(unsafe{
                std::mem::transmute::<_,u64>(#ra)
            })),
            waffle::Operator::TableGet { table_index } => {
                let t = Ident::new(&table_index.to_string(), Span::call_site());
                unop!(bl,vn,ra,args,(ctx.z().#t[#ra as usize].clone()))
            }
            waffle::Operator::TableSet { table_index } => {
                let t = Ident::new(&table_index.to_string(), Span::call_site());
                binop!(bl,vn,ra,rb,args,({
                 ctx.z().#t[#ra as usize] = #rb.clone();
                 0
                }));
            }
            waffle::Operator::TableGrow { table_index } => {
                let t = Ident::new(&table_index.to_string(), Span::call_site());
                binop!(bl,vn,ra,rb,args,({
                    for _ in 0..#rb{
                    ctx.z().#t.push(#ra.clone());
                    };
                    0
                }))
            }
            waffle::Operator::TableSize { table_index } => {
                let t = Ident::new(&table_index.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    let #vn = ctx.z().#t.len() as u32;
                }
            }
            waffle::Operator::MemorySize { mem } => {
                let m = Ident::new(&mem.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    let #vn = (ctx.#m().len() / 65536) as u32;
                }
            }
            waffle::Operator::MemoryGrow { mem } => {
                let m = Ident::new(&mem.to_string(), Span::call_site());
                let a = Ident::new(&args[0].to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    let #vn = ctx.#m().len() / 65536;
                    let l = ctx.#m().len();
                    ctx.#m().resize(l + (#a as usize) * 65536,0);
                    let #vn = #vn as u32
                }
            }
            waffle::Operator::MemoryCopy { dst_mem, src_mem } => {
                let dst_mem = Ident::new(&dst_mem.to_string(), Span::call_site());
                let src_mem = Ident::new(&src_mem.to_string(), Span::call_site());
                ternop!(bl,vn,ra,rb,rc,args,({
                    let v = ctx.#src_mem()[(#ra as usize)..][..(#rc as usize)].to_vec();
                    ctx.#dst_mem()[(#rb as usize)..][..(#rc as usize)].copy_from_slice(&v);
                    0u32
                }))
            }
            waffle::Operator::MemoryFill { mem } => {
                let mem = Ident::new(&mem.to_string(), Span::call_site());
                ternop!(bl,vn,ra,rb,rc,args,({
                    ctx.#mem()[(#ra as usize)..][..(#rc as usize)].fill((#rb & 0xff) as u8);
                    0u32
                }))
            }

            _ => panic!("unsupported operator: {}",self.0),
        };
        quote! {
            {
                #bl;
                #vn
            }
        }
    }
}