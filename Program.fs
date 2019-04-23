open AngleSharp.Html.Parser
open AngleSharp.Dom
open System
open PPrint
open System.Text

let prettySeq (l, r) separator toDoc xs =
    if Seq.isEmpty xs then
        l <^> r
    else
        let ds =
            xs
            |> Seq.choose toDoc
            |> punctuate separator
            |> vsep
        in
           l <..> ds |> nest 4 <..> r |> group

let prettyAttributes (attrs: IAttr seq) =
    attrs
    |> Seq.map (fun attr ->
        match attr.Name, attr.Value with
        | "class", v    -> (txt "attr.class'") <+> dquotes (txt v)
        | n, v          -> (txt "attr.") <^> (txt n) <+> dquotes (txt v)
    )
    |> prettySeq lrbracket semi Some

let rec prettyNodes nodes =
    prettySeq lrbracket linebreak pretty nodes

and pretty (node: INode) =
    match node with
    | :? IElement as el ->  
        let attrs =
            if not (isNull el.Attributes)
                then el.Attributes |> prettyAttributes
                else empty

        let tag = el.TagName.ToLower()

        let fstPart = (txt tag) <+> attrs

        let full =
            if tag <> "img"
                then fstPart <+> (prettyNodes el.ChildNodes)
                else fstPart 
        in
            Some full

    | _ ->
        let str = node.Text().Trim()
        if not <| String.IsNullOrEmpty str
            then str |> sprintf "text \"%s\"" |> txt |> Some
            else None
    
[<EntryPoint>]
let main _ =  
    Console.InputEncoding <- Encoding.UTF8
    Console.OutputEncoding <- Encoding.UTF8
    
    let document=
        Seq.initInfinite (fun _ -> Console.In.ReadLine())
        |> Seq.takeWhile ((<>) null)
        |> String.concat "\n"
        |> HtmlParser().ParseDocument

    document.QuerySelector("BODY").ChildNodes.[0]
    |> pretty
    |> Option.iter (render (Some 80) >> Console.Out.Write)

    0
