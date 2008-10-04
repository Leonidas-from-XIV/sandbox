#lang scribble/doc

@(require scribble/manual)

@title{worktime}

Welcome to the documentation of the worktime program. Worktime, besides being
a totaly awesome program also defines some useful functions that are worth
documenting.

@defmodule[worktime.scm]

@defproc[(port->list [lst list?])
         (listof 'lines)]{

Converts a ports contents to a list of lines which do not include the newline.}

@defproc[(generic-split-ref [lst list?] [ref number?])
         (string 