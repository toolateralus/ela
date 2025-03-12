

### Things that we need to consider to implement a drop/destructor mechanism

Most of the time, variables that need to be dropped will have this done in the declaring scope. such as

```rust
main :: fn() {
  {
    x := List!<s32>::init(.[0,1,2,3]);
    
    /* ..generated..
      x.drop()
    */
  } // <- here..
    // at the end of the block, 'x.drop()' would implicitly get called.
}
```

This works great as a standalone, and is very easy to implement. However, since we have
C like memory semantics, where everything is shallowly copied when passed as value, such as

```rust
do_some_action :: fn(mut l: List!<s32>) {
  // here, 'l' is a shallow copy. it is essentially a view of the list,
  // and if we changed the length or capacity, it would not be reflected in the original value.
  l.length = 100;
}

main :: fn() {
  x := List!<s32>::init(.[0,1,2,3]);
  // x.length == 4 here.
  do_some_action(x);
  // x.length == 4 still, even if we mutated it in 'do_some_action'
}
```

Since this is how it works, we encounter a problem: Who calls the destructor here? It may seem obvious,
you would call the destructor in `main`, right? well, let's look at a case that challenges that idea.

```rust
// same as before.
do_some_action :: fn(mut l: List!<s32>) {
  l.length = 100;
}

// now, we pass the list as an rvalue, a temporary value that is owned by the function.
main :: fn() {
  do_some_action(List!<s32>::init(.[0,1,2,3]));
}
```

in the above example, we see that the _function_ actually owns the value now. So, it would be the responsibility of the 
callee to destroy that value.


Now, let's take another look at a complex problem to solve when using a `drop/destructor` trait.

```rust
get_list :: fn() -> List!<s32> {
  local := List!<s32>::init(.[0,1,2,3]);
  return local;
}

main :: fn() {
  list := get_list();
}
```

Above, we might assume immediately (if we didn't see anything but the `get_list::local` declaration) that the `get_list` function
owns the `local` value. However, would we call a destructor on a value that is returned from a function? absolutely not, for obvious reasons.
copy functions, `construct/new` functions, etc. Things should be able to return values that implement `drop` without destroying that object
and returning a junk value.

So, the solution would be to just apply the need for a `drop` call to the recieving variable (in this case, `main::list`), right?

well, that's not as simple either.

What happens when we do this?

```rust 
get_list :: fn() -> List!<s32> {
  local := List!<s32>::init(.[0,1,2,3]);
  return local;
}

main :: fn() {
  get_list(); // we ignore the value here! does this just leak memory?
}
```

Well, then that invokes a somewhat simple solution right? just destroy any values that arent stored in variables when we return a droppable object. 

Again, it's not that simple.

```rust
get_list :: fn() -> List!<s32> {
  local := List!<s32>::init(.[0,1,2,3]);
  return local;
}

mutate_list :: fn(list: List!<s32>)  {
  // do some crap here.
}

main :: fn() {
  mutate_list(get_list());
}
```

In the above example, we clearly see that we can't destroy that value, as it goes into another function as an rvalue.

In conclusion, it's quite apparent that we can't really achieve a drop trait unless we have a comprehensive IR and ownership analyzer. This would be a net-positive, especially as we move to a LLVM backend, but it's a far ways away.