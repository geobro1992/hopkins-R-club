# R Glossary

### here are a few important terms you will often here in R conversations:

**argument**: an element of a function, either essential or optional, that informs or alters how the function works. For instance, it can be a file path where the function should import from or save to: file = \"file-path\". It can modify the colours in a plot: col = \"blue\". You can always find which arguments are taken by a function by typing ?function-nameinto the command line.

**class**: the type of data contained in a variable: usually character (text/words), numeric (numbers), integer (whole numbers), or factor (grouping values, useful when you have multiple observations for sites or treatments in your data).

**command**: a chunk of code that performs an action, typically contains one or more functions. You run a command by pressing "Run" or using a keyboard shortcut like Cmd+Enter, Ctrl+Enter or Ctrl+R

**comment**: a bit of text in a script that starts with a hashtag # and isn’t read as a command. Comments make your code readable to other people: use them to create sections in your script and to annotate each step of your analysis

**console**: the window where you can type code directly in the command line (2+2 followed by Enter will return 4), and where the outputs of commands you run will show.

**data frame**: a type of R object which consists of many rows and columns; think Excel spreadsheet. Usually the columns are different variables (e.g. age, colour, weight, wingspan), and rows are observations of these variables (e.g. for bird1, bird2, bird3) .

**function**: code that performs an action, and really how you do anything in R. Usually takes an input, does something to it, and returns an output (an object, a test result, a file, a plot). There are functions for importing, converting, and manipulating data, for performing specific calculations (can you guess what min(10,15,5) and max(10,15,5) would return?), making graphs, and more.

**object**: the building blocks of R. If R was a spoken language, functions would be verbs (actions) and objects would be nouns (the subjects or, well, objects of these actions!). Objects are called by typing their name without quotation marks. Objects store data, and can take different forms. The most common objects are data frames and vectors, but there are many more, such as lists and matrices.

**package**: a bundle of functions that provide functionality to R. Many packages come automatically with R, others you can download for specific needs.

**script**: Similar to a text editor, this is where you write and save your code for future reference. It contains a mix of code and comments and is saved as a simple text file that you can easily share so that anyone can reproduce your work.

**vector**: a type of R object with one dimension: it stores a line of values which can be character, numeric, etc.

**working directory**: the folder on your computer linked to your current R session, where you import data from and save files to. You set it at the beginning of your session with the setwd() function.

**workspace**: this is your virtual working environment, which contains all the functions of the packages you have loaded, the data you have imported, the objects you have created, and so on. It’s usually best to start a work session with a clear workspace.
