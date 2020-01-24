install.packages("fortunes")
library(fortunes)
fortune() #selects amusing quotes from R-help mailing list

install.packages("cowsay")
library(cowsay)
say("Hello world!")
say("Moo may represent an idea, but only the cow knows.\n --Mason Cooley",
    by = "cow")

#selects random animal
someone_say_hello <- function() {
  animal <- sample(names(animals), 1)
  say(paste("Hello, I'm a ", animal, ".", collapse = ""), by = animal)
}
someone_say_hello()

#If I need info about particular function in a package. In this case which values "by" can take.
?cowsay::say
?say


#Random animal + Random fortune
someone_say_my_fortune <- function(x) {
  animal <- animal <- sample(names(animals), 1)
  say(paste(fortune(), collapse = "\n"), by = animal)
}
someone_say_my_fortune()


if(!require(devtools)) {install.packages("devtools")}
devtools::install_github("brooke-watson/BRRR")
library(BRRR)
install.packages("audio")
library(audio)
skrrrahh(sound = 26)
skrrrahh()
skrrrahh(0)
skrrrahh("snoop")
skrrrahh(41)
skrrrahh("gucci")
# Play a random rap adlib.
skrrrahh(0)
# Update all packages and have Big Sean yell "Whoa Dere" when it is ready.  
update.packages(ask=FALSE); skrrrahh(10)
# Change your options to have DJ Khaled console you everytime you hit an error
# message.
options(error = function() {skrrrahh(34)})
# The ting goes: 
skrrrahh(11)

install.packages("praise")
library(praise)
praise()

