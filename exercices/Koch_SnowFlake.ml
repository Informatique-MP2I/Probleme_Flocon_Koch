open Graphics
open Graphics
    
let draw_line ()=
  open_graph " 750x750";
  set_color blue;
  set_line_width 2;
  moveto 300 300;
  lineto 400 400;
  ignore (wait_next_event [Key_pressed]);
  close_graph ()

        
let () =
  Printf.printf "Koch SnowFlake\n";  
  draw_line ()
