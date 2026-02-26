// simple let with add in body with let
let
  ni x is 5
in
  x + let
         ni y is 3
      in
         x + y
      end
end
