open Graphics

let rec draw_Koch_line xs ys xe ye deep =
  if deep = 0 then
    begin
      moveto xs ys;
      lineto xe ye
    end
  else
    let x1 = (2*xs + xe) / 3 in
    let x1f = float_of_int x1 in 
    let y1 = (2*ys + ye) / 3 in
    let y1f = float_of_int y1 in
    let x2 = (xs + 2*xe) / 3 in
    let x2f = float_of_int x2 in
    let y2 = (ys + 2*ye) / 3 in
    let y2f = float_of_int y2 in
    let x3f = (x1f +. x2f +. (sqrt 3.) *. y1f -. (sqrt 3.) *. y2f) /. 2. in
    let x3 = int_of_float x3f in
    let y3f = ((-1.) *. sqrt(3.) *. x1f +. sqrt(3.) *. x2f +. y1f +. y2f) /. 2. in
    let y3 = int_of_float y3f in
    draw_Koch_line xs ys x1 y1 (deep-1);
    draw_Koch_line x1 y1 x3 y3 (deep-1);
    draw_Koch_line x3 y3 x2 y2 (deep-1);
    draw_Koch_line x2 y2 xe ye (deep-1)

let draw_Koch_SnowFlake n =
  open_graph " 750x750";
  set_color blue;
  set_line_width 2;
  
  (* draw_Koch_line 75 200 675 200 n; *)
  draw_Koch_line 75 200 375 720 n;
  draw_Koch_line 375 720 675 200 n;
  draw_Koch_line 675 200 75 200 n;
  
  ignore (wait_next_event [Key_pressed]);
  close_graph ()
        
let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Usage : %s <integer>\n" Sys.argv.(0)
  else
    begin
      Printf.printf "Koch SnowFlake\n";
      let n = int_of_string Sys.argv.(1) in
      draw_Koch_SnowFlake n
    end
