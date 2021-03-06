# Reason for fun (and profit)

In this tutorial / quick-start / notes, I'm going to be exploring [Reason](https://facebook.github.io/reason/). So why not Elm or PureScript? Both languages are super cool but both have some parts I'm not as excited about. PureScript uses Bower -- ew! And neither have easy interoperability with external JavaScript libraries. I had a conversation with @chenglou and he absolutely sold me on Reason. So here goes nothing!

## Getting Started

If you're getting started from absolutely nothing on a Mac, the following will get you set up. But it's likely you've already done this:

```sh
# install developer tools
xcode-select --install
# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# install node and npm
brew install node
# install atom editor
brew cask install atom
```

These are the necessary tools that are specific to Reason that you probably don't have yet:

```sh
# install ninja
brew install ninja
# install and build the reason cli tools
npm install -g git://github.com/reasonml/reason-cli.git
# install the atom editor plugin along with a bunch of other facebook tooling
apm install nuclide
```

Nuclide is going to mess with your Atom UI so you can go to `Settings > Packages`, search for "nuclide" and change the Nuclide Settings. I would try to leave as much there as possible -- you don't want to get stuck on this part trying to get your IDE to work. But if you must, somewhere towards the middle is a "Use" section with a bunch of check boxes. I *believe* you can turn everything off except for the following:

```
hyperclick
nuclide-code-format
nuclide-datatip
nuclide-definition-hyperclick
nuclide-language-reason
nuclide-ocaml
nuclide-type-hint
```

A few customizations I would recommend:

- go to the `nuclide-code-format` section and tick "format on save".
- go to the `hyperclick` section and change the the command to `control-click` so you can still use command-click for multiple cursors.

At the end of the day, there are 4 editor features that you should **make sure** are working:
- When you type some garbage syntax, there should be red lines with compile errors.
- When you hover over a variable (where you cursor isn't), a datatip shows up with the type signature of that variable.
- When you control-click on a variable, it will jump to the definition of that variable.
- When you change the whitespace in the file and hit save, everything reformats to the way it was before.

You might notice some errors in the editor already even with a fresh project. That's because the editor tools depend on the artifacts generated by the build system. So let's get this project running:

```sh
npm install
npm start
# in another tab
npm run build
# in another tab
open src/index.html
```

This build system is actually two separate tools. We're using `bucklescript` to compile Reason => OCaml => JavaScript, and we're using `webpack` to bundle all the files together for the browser.

## Learning Reason

Reading through the docs is an invaluable experience:

- [Reason documentation](http://facebook.github.io/reason/index.html)
  - Read the [modules section](http://facebook.github.io/reason/modules.html) twice!
  - Read the [JavaScript comparison section](http://facebook.github.io/reason/javaScriptCompared.html) as well.
- [Rehydrate documentation](https://github.com/glennsl/reason-react-quick-start/blob/master/quick-start.md) (for using React with Reason)
- [BuckleScript Readme](https://github.com/bloomberg/bucklescript) and the [BuckleScript documentation](https://bloomberg.github.io/bucklescript/Manual.html) (for JS interop)
- [OCaml docs](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html) (for some of the core function like `sin` and `cos`)

## Examples

Check out the [`src/tutorial.re`](./src/tutorial.re). I've built up a few different examples that you can check out in your browser.

Here are some other places you can find some examples:
- [Rehydrate Examples](https://github.com/chenglou/rehydrate-example/tree/master/src)
- [Redux Example](https://github.com/rickyvetter/rehydrate/tree/redux/examples/redux)
- [Another Quickstart](https://github.com/glennsl/rehydrate-quick-start/blob/master/quick-start.md)

## Notes / Questions

- What's the difference between an abstract type, a polymorphic type, and a generic type?

- What are the `unit` and `_` types called? What are they used for?

- Leveraging the syntax for fun an profit.

    I need a function that gets the first `n` items off of a list. My first shot looked like this:

    ```re
    let rec firstN n l =>
      if (n === 0 || List.length l === 0) {
        []
      } else {
        let h = List.hd l;
        let t = List.tl l;
        List.append [h] (firstN (n - 1) t)
      };
    ```

    But we can do a lot better. We can pattern match on `n` and `l`, destructure the `hd` and `tl`, and spread into a list rather than using `append`:

    ```re
    let rec firstN n l =>
      switch (n, l) {
      | (0, _) => []
      | (_, []) => []
      | (n, [h, ...t]) => [h, ...firstN (n - 1) t]
      };
    ```
- `[%bs.debugger]` compiles to `debugger` in the browser. You won't see the source mapped back to Reason, but the JavaScript is actually *very* readable!

# Questions

- What if `instanceVars` weren't mutable but were instead returned from the updater methods along with the state? Maybe call it a `returnBag`.

- It seems like [`memoizedUpdaterCount` is unused](https://github.com/reasonml/rehydrate/blob/06c409d3fb6334f79cd1e8b9d9916bd8d3d80e84/src/reactRe.re#L282) since you're using `maxMemoizedCount` when we make the array.

- Where can I read about `Obj.magic`, `Js.Null.return`?

- Can I somehow use Webpack's `import()` for code splitting and async module loading?

- How should I handle compiled assets? I have markdown files that I want to parse at compile-time to generate React components. Should I just use a Makefile to generate `.re` files that are `.gitignore`'d? Or are there any compile-time facilities?

- How does publishing Reason packages work?

- Can I use native OCaml libraries? Where are they? How can I use them? Will they compile to JavaScript? How can I build a native server that doesn't compile to JavaScript?

- How can I write some simple unit test?
