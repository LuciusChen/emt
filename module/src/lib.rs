#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use jieba_rs::Jieba;
use libc_alloc::LibcAlloc;
use std::ffi::CString;
use std::os::raw;
use std::ptr;
use std::sync::LazyLock;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

static JIEBA: LazyLock<Jieba> = LazyLock::new(Jieba::new);

#[unsafe(no_mangle)]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 1;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn emacs_module_init(runtime: *mut emacs_runtime) -> libc::c_int {
    unsafe {
        let env = (*runtime).get_environment.unwrap_unchecked()(runtime);

        let intern = (*env).intern.unwrap_unchecked();
        let funcall = (*env).funcall.unwrap_unchecked();
        let make_function = (*env).make_function.unwrap_unchecked();

        let Qfset = intern(env, c"fset".as_ptr());

        // emt--segment / emt--do-split-helper
        let Qsegment = intern(env, c"emt--segment".as_ptr());
        let Qsplit = intern(env, c"emt--do-split-helper".as_ptr());
        let fn_segment = make_function(
            env, 1, 1, Some(Femt__do_split_helper),
            c"Split TEXT into word bounds using jieba.".as_ptr(),
            ptr::null_mut(),
        );
        funcall(env, Qfset, 2, [Qsegment, fn_segment].as_mut_ptr());
        funcall(env, Qfset, 2, [Qsplit, fn_segment].as_mut_ptr());

        0
    }
}

unsafe extern "C" fn Femt__do_split_helper(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    unsafe {
        debug_assert_eq!(nargs, 1);
        let intern = (*env).intern.unwrap_unchecked();
        let funcall = (*env).funcall.unwrap_unchecked();
        let make_integer = (*env).make_integer.unwrap_unchecked();

        let Qcons = intern(env, c"cons".as_ptr());
        let Qvector = intern(env, c"vector".as_ptr());

        let text = copy_string(env, args).unwrap_unchecked();
        let words = JIEBA.cut(&text, false);

        let mut pos = 0i64;
        let mut cells: Vec<emacs_value> = Vec::with_capacity(words.len());
        for w in &words {
            let l = pos;
            let r = pos + w.chars().count() as i64;
            pos = r;
            let lv = make_integer(env, l);
            let rv = make_integer(env, r);
            cells.push(funcall(env, Qcons, 2, [lv, rv].as_mut_ptr()));
        }

        funcall(env, Qvector, cells.len() as isize, cells.as_mut_ptr())
    }
}

fn copy_string(env: *mut emacs_env_29, args: *mut *mut emacs_value_tag) -> Result<String, ()> {
    unsafe {
        let copy_string_contents = (*env).copy_string_contents.unwrap_unchecked();
        let mut len: isize = 0;
        let is_ok = copy_string_contents(env, *args, ptr::null_mut(), &mut len);
        if !is_ok {
            return Err(());
        }
        let mut buf = vec![0u8; len as usize];
        let is_ok =
            copy_string_contents(env, *args, buf.as_mut_ptr() as *mut raw::c_char, &mut len);
        if !is_ok {
            return Err(());
        }
        Ok(String::from_utf8_unchecked(
            CString::from_vec_with_nul_unchecked(buf).into_bytes(),
        ))
    }
}
