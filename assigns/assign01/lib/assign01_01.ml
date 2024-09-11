let rec pow base exponenet = 
  if exponenet = 0 then 1
  else base * pow base (exponenet - 1)