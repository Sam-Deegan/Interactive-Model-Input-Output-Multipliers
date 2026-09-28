# Conventions

How the interactive model apps present a model to a student. The code refers
to these sections by number.

## 1. One decision per control

The sidebar chooses the model (the stage). The main window chooses what to
run in it (the worked example). A control at the top of a tab governs the
whole tab; a control for one figure sits in that figure's card header.

## 2. There is no "Custom"

Every stage opens on its own first worked example and the sliders stay live
throughout. A student plays by moving them. Reset returns the defaults.

## 3. The story is one paragraph

Three or four sentences, in this order: what changes and why (the economic
source of the shock, which curve it moves, which way); what that does to
both axes of the diagram, and whether they move together or apart; what
decides how far and how long, and what to watch.

## 4. Name the variable and its symbol together

At first mention, in brackets: "output (y) falls below potential (y*)", not
"the output gap opens". Once named, the bare symbol is fine. Give a number
only where the preset sets it and it is part of what the example is.

## 5. A star is a resting point, not an optimum

y* is potential output, the level at which inflation is stable; r* is the
natural real rate. Neither is chosen by an optimiser. The one star that is a
choice is the reset price p* in the Calvo model, and the app says so there.

## 6. What goes on a figure

- Name the curve in full with its abbreviation in brackets the first time:
  Investment-Saving (IS) Curve, Phillips Curve (PC), Monetary Policy (MP)
  Rule. Use the same words in the card header, the equations panel and the
  prose.
- A curve carries its name, not its equation, set horizontally at the end of
  the line in the line's own colour. The equation belongs beside the figure.
- Two states of one curve each name the parameter that separates them, in
  brackets: IS-MP (epsilon^y = 0) against IS-MP (epsilon^y > 0). Both are
  drawn solid in their own colour, with arrows from the old position to the
  new. A named pair needs no legend.
- Zero is a faint dashed cross on both axes. A resting point (r*, pi*, a
  zero gap) is dotted and darker. Both are drawn before the curves.
- A value is marked with dashed leaders from each axis to the point, and the
  symbol is a break on the primary axis, not a secondary-axis tick.
- Axis titles are the word, then the symbol in brackets: Inflation (pi_t).
  Bold, at the subtitle size; a plotmath expression needs bold() itself.
- Gridlines off, axes on: T_02_01_theme_fn(grid = "h") for a series read off
  the y axis, "v" for a horizontal bar chart, "none" for a diagram.
- No title or subtitle inside a figure. The card header and the caption
  under the card do that job.
- A vertical distance between two values is a distance marker (a thin line
  with a short cap at each end, labelled beside it), never a shaded band. A
  light shade with a name inside it marks a genuine region of the plane.
- Dynamics are arrows: along the axis towards a steady state, or from the
  old curve to the new one.
- Reading notes go under the figure, never inside the image.
- Figures sit in half-width pairs at 3:2. Non-ASCII in R/*.R string
  constants is written as a \uXXXX escape, because shinylive runs under a
  C locale.

## 7. Register

Terse. Sentence case for prose, Title Case for headings and buttons. The
palette has no red and no amber: series in navy, comparisons dashed in
green, bands in light blue, reference lines grey dashed.

## 8. The ghost

Every figure can draw itself twice: at the sliders, and at the loaded worked
example's own settings. The second is the ghost, a faint copy drawn first,
so a student who moves a slider can see where the curve was without having
asked for a comparison. When the two agree the ghost is not drawn.
