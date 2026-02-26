// simple let with add and let in parens
let
  ni x is 5
in
  x + (let
         ni y is 3
       in
         x + y
       end)
end
