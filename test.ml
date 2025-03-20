let read_file fileName = In_channel.with_open_bin fileName In_channel.input_all

let read_lines file = let contents = In_channel.with_open_bin file In_channel.input_all in 
    (String.split_on_char '\n' contents);;

let write_file file s =
    Out_channel.with_open_bin file
        (fun oc -> Out_channel.output_string oc s)

let byte_ic = (In_channel.open_bin) "test.jpg";;

let rec countSize ?(prev=0) count ic =
    let b = In_channel.input_byte ic in
    match b with
    | None -> count 
    | Some x -> if(count < 50) then (print_string (" " ^ string_of_int prev ^ " "); countSize ~prev:x (count + 1) ic) else countSize ~prev:x (count + 1) ic

let rec debugcountSize ?(prev=0) count ic = 
    let b = In_channel.input_byte ic in
    match b with
    | None -> count
    | Some 216 -> if (prev == 255) then (print_endline "Start of File."; debugcountSize ~prev:216 (count + 1) ic ) else debugcountSize ~prev:216 (count + 1) ic
    | Some 224 -> if (prev == 255) then (print_endline "Default header."; debugcountSize ~prev:224 (count + 1) ic) else debugcountSize ~prev:224 (count + 1) ic
    | Some 219 -> if (prev == 255) then (print_endline "Quantization Table."; debugcountSize ~prev:219 (count + 1) ic) else debugcountSize ~prev:219 (count + 1) ic
    | Some 192 -> if (prev == 255) then (print_endline "Start of Frame."; debugcountSize ~prev:192 (count + 1) ic) else debugcountSize ~prev:192 (count + 1) ic
    | Some 196 -> if (prev == 255) then (print_endline "Huffman Table."; debugcountSize ~prev:196 (count + 1) ic) else debugcountSize ~prev:196 (count + 1) ic
    | Some 218 -> if (prev == 255) then (print_endline "Start of scan."; debugcountSize ~prev:218 (count + 1) ic) else debugcountSize ~prev:218 (count + 1) ic
    | Some 217 -> if (prev == 255) then (print_endline "End of File."; debugcountSize ~prev:217 (count + 1) ic) else debugcountSize ~prev:217 (count + 1) ic
    | Some x -> debugcountSize ~prev:x (count + 1) ic
    

(*Some 216 -> if(prev = 255) then (print_string "JPG"; (countSize ~prev:217 (count + 1))) else (countSize ~prev:217 (count + 1))
    | Some 217 -> if(prev = 255) then count else (countSize ~prev:217 (count + 1))*)


let ic2 = (In_channel.open_bin) "test2.jpg"

let start_of_file = Bytes.of_string "\xFF\xD8"
let start_of_header = Bytes.of_string "\xFF\xE0"
let start_of_comment = Bytes.of_string "\xFF\xFE"
let start_of_quantization = Bytes.of_string "\xFF\xDB"
let start_of_frame = Bytes.of_string "\xFF\xC0"
let start_of_huffman = Bytes.of_string "\xFF\xC4"
let start_of_scan = Bytes.of_string "\xFF\xDA"
let end_of_file = Bytes.of_string "\xFF\xD9"

let tophalf num = match num with
    | 15 -> "F"
    | 14 -> "E"
    | 13 -> "D"
    | 12 -> "C"
    | 11 -> "B"
    | 10 -> "A"
    | x when (x < 10) -> string_of_int x
    | _ -> ""

let int_to_hex num =
    ((land) num 240 lsr 4 |> tophalf) ^ ((land) num 15 |> tophalf)

let print_hex bytes = 
    Bytes.iter (fun c -> (print_string @@ (c |> int_of_char |> int_to_hex) ^ " ")) bytes; print_newline ()

(*Consume and read the length after the header. return the length*)
let readLength ic =
    let bits = Bytes.create 2 in
    ignore @@ In_channel.input ic bits 0 2;
    Bytes.get_uint16_be bits 0

(*Start reading.*)
let rec readHeader ic = 
    let bits = Bytes.create 2 in
    ignore @@ In_channel.input ic bits 0 2;
    match bits with
    | b when (Bytes.equal b start_of_file) -> print_endline "Start of File"; readHeader ic (*The next value after start of file is the default header.*)
    | b when (Bytes.equal b start_of_header) -> print_endline "Start of Header"; readLength ic |> discardData ic
    | b when (Bytes.equal b start_of_comment) -> print_endline "Start of Comment."; readLength ic |> discardData ic
    | b when (Bytes.equal b start_of_quantization) -> print_endline "Start of Quantization"; readLength ic |> discardData ic
    | b when (Bytes.equal b start_of_frame) -> print_endline "Start of Frame"; readLength ic |> discardData ic
    | b when (Bytes.equal b start_of_huffman) -> print_endline "Start of Huffman Table"; readLength ic |> readHuffman ic
    | b when (Bytes.equal b start_of_scan) -> print_endline "Start of Scan"; readLength ic |> discardData ic
    | b when (Bytes.equal b end_of_file) -> print_endline "End of file."; () (*This is the end of the recursion.*)
    | _ -> failwith ("potato." ^ Bytes.to_string bits)


(*Consume and read the data for each header here. Discard it for now make sure to read the data though.*)
and discardData ic c =
    if c == 2 then readHeader ic else (ignore @@ In_channel.input_byte ic; discardData ic (c-1))

and readHuffman ic length =
    let huffmanBytes = Bytes.create (length-2) in
    ignore @@ In_channel.input ic huffmanBytes 0 (length-2);
    print_endline @@ "Length: " ^ Int.to_string (length-2);
    print_hex huffmanBytes;
    readHeader ic

    
let bbt = Bytes.of_string "\x00\x03\x00\x02\x03\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00"

let huffmantest =  Bytes.fold_right (fun c a -> int_of_char c :: a) bbt []

let rec tail a = match a with
    | [] -> failwith "Empty List"
    | h :: [] -> h
    | h :: t -> tail t

(*Calculates the huffman codes from the frequency. These codes can then be mapped to the symbols to translate them.*)
let rec calcCodes ?(prev=0) lst =
    match lst with
    | [] -> []
    | h :: t when (h = 0) -> calcCodes ~prev:(prev lsl 1) t
    | h :: t -> let front : 'a list = calcCode prev h in front @ (calcCodes ~prev:((tail front) lsl 1) t)

and calcCode p n =
    match n with
    | 0 -> []
    | x ->  p :: calcCode (p+1) (x - 1)


