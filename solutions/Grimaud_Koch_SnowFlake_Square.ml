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
    let x3f = x2f +. y2f -. float_of_int ye in
    let x3 = int_of_float x3f in
    let y3f = y2f +. float_of_int xe -. x2f in
    let y3 = int_of_float y3f in
    let x4f = x1f +. y1f -. y2f in
    let x4 = int_of_float x4f in
    let y4f = y1f +. x2f -. x1f in
    let y4 = int_of_float y4f in
    draw_Koch_line xs ys x1 y1 (deep-1);
    draw_Koch_line x1 y1 x4 y4 (deep-1);
    draw_Koch_line x4 y4 x3 y3 (deep-1);
    draw_Koch_line x3 y3 x2 y2 (deep-1);
    draw_Koch_line x2 y2 xe ye (deep-1)

let rec length_Koch_line deep l =
  if deep = 0 then
    l
  else
    5.*.(length_Koch_line (deep-1) (l/.3.))

let draw_Koch_SnowFlake n =
  open_graph " 750x750";
  set_color blue;
  set_line_width 2;
  
  draw_Koch_line 75 200 675 200 n;
  
  ignore (wait_next_event [Key_pressed]);
  close_graph ()
    
let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Usage : %s <integer>\n" Sys.argv.(0)
  else
    begin
      let n = int_of_string Sys.argv.(1) in
      Printf.printf "Koch SnowFlake Square - length:%f\n" (length_Koch_line n 9.);
      draw_Koch_SnowFlake n
    end
