# WorldCupSweep

A Shiny app for managing a Football World Cup (or similar) sweepstake.

Simple winner-takes-all sweepstakes are boring. As soon as someone gets a crappy team out of the hat they instantly lose interest. A better way to do include everyone is to have secondary prizes for 'bad' perfomances: e.g. most goals conceded, most yellow cards collected. This keeps interest going into the second week and beyond for anyone who doesn't have Brasil/Spain/etc.

The problem is tracking that kind of information.

This is where this R Shiny app comes in. It helps in three ways:

  1. Allows inputting (and editing) of results directly
  2. Stores all the data locally
  3. Plots the data for easy reporting

# Requirements

  * R Studio
  * R
  * Shiny
  * Shinyjs
  * ggplot
  * RSQLite (+ DBI)
  * DT

# How to run

Clone the repository, open the 'app.R' script in Rstudio and hit the 'Run App' button.
