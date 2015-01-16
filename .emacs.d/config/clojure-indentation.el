;;;;;
;; Custom indentation for clojure(script)
;;;;;

(defvar my-funcs
	'(
		row
		))

(defvar om-fn-symbols
	'(
		init-state
		will-mount
		did-mount
		should-update
		will-receive-props
		will-update
		did-update
		render
		render-state
		display-name
		will-unmount
		build
		build-all
		))

(defvar my-dom-tags
  '(
		a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    big
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    data
    datalist
    dd
    del
    dfn
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h1
    h2
    h3
    h4
    h5
    h6
    head
    header
    hr
    html
    i
    iframe
    img
    ins
    kbd
    keygen
    label
    legend
    li
    link
    main
    map
    mark
    marquee
    menu
    menuitem
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    output
    p
    param
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    script
    section
    select
    small
    source
    span
    strong
    style
    sub
    summary
    sup
    table
    tbody
    td
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr
    
    ;; svg
    circle
    ellipse
    g
    line
    path
    polyline
    rect
    svg
    text
    defs
    linearGradient
    polygon
    radialGradient
    stop
    tspan))

(dolist (tag my-dom-tags)
	(put-clojure-indent tag 1))

(dolist (tag om-fn-symbols)
	(put-clojure-indent tag 1))

;;TODO: This is replicated because neither nested dolists nor concat
;;seemed to work. Why ever not?
(dolist (tag my-funcs)
	(put-clojure-indent tag 1))

