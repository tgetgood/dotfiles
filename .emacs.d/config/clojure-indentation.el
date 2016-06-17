;;;;;
;; Custom indentation for clojure(script)
;;;;;

(defvar my-put-1
	'(
		accumulate
		async
		for-all
		row
		this-as
		trow
		))

(defvar my-put-2
	'(
		authorized?
		respond-to
))

(defvar om-fn-symbols
	'(
		build
		build-all
		did-mount
		did-update
		display-name
		init-state
		render
		render-state
		should-update
		will-mount
		will-receive-props
		will-unmount
		will-update
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
    defs
    ellipse
    g
    line
    linearGradient
    path
    polygon
    polyline
    radialGradient
    rect
    stop
    svg
    text
    tspan))


(dolist (tag my-dom-tags)
	(put-clojure-indent tag 1))

(dolist (tag om-fn-symbols)
	(put-clojure-indent tag 1))

;;TODO: This is replicated because neither nested dolists nor concat
;;seemed to work. Why ever not?
(dolist (tag my-put-1)
	(put-clojure-indent tag 1))

(dolist (tag my-put-2)
	(put-clojure-indent tag 2))

(defun paredit-space-for-reader-conditional (endp delim)
	"Do not insert a space between #? and ("
	(or endp
			(cond ((eq (char-syntax delim) ?\()
						 (not (looking-back (regexp-quote "#?") 2 nil)))
						(else t))))

(add-hook 'clojurec-mode-hook
					(lambda ()
						(add-to-list
						 'paredit-space-for-delimiter-predicates
						 'paredit-space-for-reader-conditional)))

