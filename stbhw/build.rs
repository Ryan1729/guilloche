fn main() {
    // Tell cargo to invalidate the built crate whenever the wrapper changes
    println!("cargo:rerun-if-changed=wrapper.h");
    println!("cargo:rerun-if-changed=wrapper.c");

    cc::Build::new()
        .file("wrapper.c")
        .compile("wrapper");
}