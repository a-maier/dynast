use vergen_gitcl::{Emitter, GitclBuilder};

fn main() {
    let gitcl = GitclBuilder::default()
        .branch(true)
        .sha(true)
        .build()
        .unwrap();
    // If this is not run inside a git repository we get an error.
    // This happens when installing the crate via cargo.
    // As a quick fix, we just ignore it.
     let _ = Emitter::default()
        .add_instructions(&gitcl).unwrap()
        .emit();
}
