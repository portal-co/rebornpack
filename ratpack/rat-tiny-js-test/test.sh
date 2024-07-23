set -e
cd $(dirname $0)

cargo run -p rat-tiny-js-test ./test.js
