{- CS410 Advanced Functional Programming -}

{- Cropping and Overlaying Sized Blocks of Data -}

module Overlay where

import Block

{- Today, let's think about foreground and background.
Windows overlap. Menus drop over windows. Scrolling panes
give access to only a fragment of a display. We need to
be able to build things up in layers. Correspondingly,
we need to crop out the prats we can actually see.
-}

{- First, let's get cropping. -}

{- What to crop out? Let's figure out how to crop a
   rectangular region out of a layout. A region could
   be specified by the coords of its top left corner
   and its size. -}

type Region = (Point, Size)
type Point = (Int, Int) -- (0, 0) is (left, top)


{- Let's figure out how to crop a box, assuming the region
   fits inside. -}

cropBox :: (Point, Size) -> Box -> Box
cropBox ((x, y), (w, h)) = map (take w . drop x) . (take h . drop y)

{- The usual pattern: if we know how to chop the stuff, then
   we can show how to crop a layout built from the stuff.
   Let us be the best of citizens, offering much and presuming
   little. What are the issues? -}

cropLay :: ((Point, Size) -> a -> a) ->
           (Point, Size) -> Layout a -> Layout a
cropLay croppa ((x, y), (w, h)) l@((r, b), _)
  | x + w <= 0 || x >= r || y + h <= 0 || y >= b  -- missed completely!
  = blank (w, h)
  | x < 0       -- off to the left
  = hGap (negate x) `joinH` cropLay croppa ((0, y), (w + x, h)) l
  | x + w > r   -- off to the right
  = cropLay croppa ((x, y), (r - x, h)) l `joinH` hGap (x + w - r)
  | y < 0       -- off to the top
  = vGap (negate x) `joinV` cropLay croppa ((x, 0), (w, h + y)) l
  | y + h > b   -- off to the bottom
  = cropLay croppa ((x, y), (w, b - y)) l `joinV` vGap (y + h - b)
cropLay croppa ps l = cropIn ps l where -- now we're in, stay in!
  cropIn (p, s) (_, Blank)    = blank s
  cropIn (p, s) (_, Stuff a)  = (s, Stuff (croppa (p, s) a))
  cropIn ((x, y), (w, h)) (_, Hor l@((lw, _), _) r)
    | x + w <= lw  -- all left!
    = cropIn ((x, y), (w, h)) l
    | x >= lw      -- all right!
    = cropIn ((x - lw, y), (w, h)) r
    | otherwise    -- bits of both, stuck back together
    = cropIn ((x, y), (lw - x, h)) l `joinH` cropIn ((0, y), (w + x - lw, h)) r
  cropIn ((x, y), (w, h)) (_, Ver t@((_, th), _) b)
    | y + h <= th  -- all top!
    = cropIn ((x, y), (w, h)) t
    | y >= th      -- all below!
    = cropIn ((x, y - th), (w, h)) b
    | otherwise    -- bits of both, stuck back together
    = cropIn ((x, y), (w, th - y)) t `joinV` cropIn ((x, 0), (w, h + y - th)) b

data Template = Hole | Ready Box

{- If we have only one layer, we could turn Holes into Blanks. -}

holeBlank :: Layout Template -> Layout Box
holeBlank (xy, Stuff Hole)       = (xy, Blank)
holeBlank (xy, Stuff (Ready b))  = (xy, Stuff b)
holeBlank (xy, Blank)            = (xy, Blank)
holeBlank (xy, Hor l r)          = (xy, Hor (holeBlank l) (holeBlank r))
holeBlank (xy, Ver t b)          = (xy, Ver (holeBlank t) (holeBlank b))

{- If we have a regular layout, we can turn it into a template
with no holes. -}

unholey :: Layout Box -> Layout Template
unholey (xy, Stuff a)    = (xy, Stuff (Ready a))
unholey (xy, Blank)      = (xy, Blank)
unholey (xy, Hor l r)    = (xy, Hor (unholey l) (unholey r))
unholey (xy, Ver t b)    = (xy, Ver (unholey t) (unholey b))

{- Hang on a minute! -}

transform :: ((Size, a) -> Layout b) -> Layout a -> Layout b
transform = undefined

{- Here's a wee holey example. -}

holeyEx :: Layout Template
holeyEx = unholey (layChar '*' (40, 5)) `joinV`
          (unholey (layChar '*' (10, 10)) `joinH` ((20,10), Stuff Hole) `joinH`
             unholey (layChar '*' (10, 10))) `joinV`
          unholey (layChar '*' (40, 5))

myString :: String
myString = "Once upon a time, there was a beautiful\n" ++
           "princess. But before she was a beautiful\n" ++
           "princess, she was a baby princess, and like\n" ++
           "all baby princesses, she had a Fairy Christening.\n" ++
           "\n" ++
           "All of the Fairies were invited, but sadly,\n" ++
           "the valet whose duty it was to post the invitations\n" ++
           "accidentally dropped one in the river whilst tripping\n" ++
           "over a loose plank, crossing the footbridge."

myLines :: [String]
myLines = lines myString

someChat :: Layout Template
someChat = unholey (foldr (joinV . layS) layZ myLines)

moreChat :: Layout Template
moreChat = someChat `joinV` vGap 1 `joinV` someChat

{- Now let's figure out how to overlay one region on top of
another, seeing through the holes. We had better be able to
crop the stuff. What are the issues? -}

overlay ::  (Point, Size) ->
            Layout Template -> Layout Template -> Layout Template
overlay = undefined


cropTemp :: (Point, Size) -> Template -> Template
cropTemp = undefined

