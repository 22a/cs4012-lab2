# CS4012: Lab 2

### An Interpretive Dancer :dancer:

##### This document is written with github flavoured markdown in mind. It is best viewed [on github, if convenient, please read it there](https://github.com/22a/cs4012-lab2).


## Excuses
I haven't completed this lab fully and I do not intend to before the submission date so I'm submitting it in its current state for fear of this niggling its way into my brain and forcing me to neglect exam study.

If it is any consolation, I know how I would complete it, commentary on this is in the relevant section below.


## Milestones

- [x] Stack
- [x] Step
- [x] Inspect
- [x] Inspect History
- [ ] Step Backwards
- [ ] Static Analysis


## Architecture
This solution builds upon the latest version of the in class interpreter with all the high fructose corn syrup removed.

The stepping is implemented by lifting blocking readLine IO actions into the run monad after each command from the user.

The variable version history is implemented by changing the environment/state from a map to a list of maps. This effectively has a snapshot of the program variable's state for every statement that changes the program's state. This, combined with a fold and a function to remove adjacent duplicates generates a history of all the values all the variables have had.


## Section Completion

##### 1. Stack :+1: 100%

##### 2. Step :+1: 100%

##### 3. Inspect :+1: 100%

##### 4. Inspect History :+1: 100%

##### 5. Step Backwards :-1: 0/attempt_marks%?
Having completed the previous step using snapshots this means I can easily restore the state of the variables of the program. The main issue I ran into was with the Statement structure, the recursive structure of the seq statement expansion made it tricky to think about restoring previous statements. I've seen classmates do cool things with lists of stats instead of recursive definitions which I know works but I've elected to not devote any more time to it.

For a while I also considered storying each statement(string of show) in the program's state stack alongside the variable values, so that I could easily just "pop" the most recent executed statement from the stack and place it into the path of function calls, but when trying to implement this I ran into an issue with how I transparently hide the expansion of Seq statements from the user. This left me with missing statements, there's a crude illustration below:

Say for example our program had 2 real statements `egg` and `chicken`, our source would look like:
```haskell
Seq (egg) (chicken)
```

My interpreter's input loop thing takes the root statement and executes it normally if it is a `Seq`, this is all well and good because if we have a really nested program we don't want to have to step through 10 sequencing statements before we get to real computation. This works great but after we "Step" through the first statement, `egg`, we're left with the `egg` statement in the program's state, and the `chicken` statement sitting in the recursive exec call tree. This works great if we never try to step backwards but when we do, and we restore the `egg` statement, we lose all notion of the `chicken` statement.

And if we were to store all the Seq statements in the program's state we'd then have to manually step forward and backward through each step.

**TL;DR:** I tried a few things, they didn't work.


##### 6. Static Analysis :-1: 0%

Having spent too long thinking about the above, I haven't tackled this yet. I may still get a chance to.


## Setup

First, clone the repo:
```bash
git clone git@github.com:22a/cs4012-lab2.git && cd cs4012-lab2
```

Setup stack:
```bash
stack setup
```

Build the stack project:
```bash
stack build
```

Edit the source file to be interpreted if you want:
```bash
nvim input.peter
```

Run the interpreter:
```bash
stack exec cs4012-lab2-exe
```

## Caveats
I tested this program inside `stack ghci`, reloading the file when there were changes, instead of building and running `stack exec` every time. For some weird reason the prompt gets pushed up to the line above in the exec execution environment. My point being, the prompt isn't really a prompt if you run in stack exec, if you'd like the unadulterated interpreting experience, after building, run:

```bash
stack ghci
```
then:
```
main
```

## Usage
 The interpreter has 5 possible commands, 4 useful, and 1 tragically unimplemented:
 * **S** : *Step* : Execute the next statement
 * **SB** : *Step Back* : Do nothing :'(
 * **IC** : *Inspect Current* : Display the current state of the program's variables
 * **IH** : *Inspect History* : Display the previous states "
 * **Q** : *Quit* : Exit the interpreter

When you start the interpreter you'll be greeted with the first non Seq statement, you can read it, and then decide what you want to do with it.

The prompt is very annoying in that, if you make a mistake typing the command, you can't backspace. I have made this slightly more bearable by making the program interpret the garble as an unknown command and ask for another.
