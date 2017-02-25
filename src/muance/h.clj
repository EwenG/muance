(ns muance.h
  (:refer-clojure :exclude [map meta time set symbol use filter])
  (:require [clojure.set]
            [muance.core :as m]))

(def html-elements #{'a 'abbr 'acronym 'address 'applet 'area 'article 'aside 'audio 'b 'base
                     'basefont 'bdi 'bdo 'big 'blockquote 'body 'br 'button 'canvas 'caption
                     'center 'cite 'code 'col 'colgroup 'datalist 'dd 'del 'details 'dfn
                     'dialog 'dir 'div 'dl 'dt 'em 'embed 'fieldset 'figcaption 'figure 'font
                     'footer 'form 'frame 'frameset 'h1 'h2 'h3 'h4 'h5 'h6 'head 'header
                     'hr 'html 'i 'iframe 'img 'input 'ins 'kbd 'keygen 'label 'legend 'li
                     'link 'main 'map 'mark 'menu 'menuitem 'meta 'meter 'nav 'noframes
                     'noscript 'object 'ol 'optgroup 'option 'output 'p 'param 'picture 'pre
                     'progress 'q 'rp 'rt 'ruby 's 'samp 'script 'section 'select 'small
                     'source 'span 'strike 'strong 'style 'sub 'summary 'sup 'table 'tbody
                     'td 'textarea 'tfoot 'th 'thead 'time 'title 'tr 'track 'tt 'u 'ul 'var
                     'video 'wbr})

(def svg-elements #{'a 'altGlyph 'altGlyphDef 'altGlyphItem 'animate 'animateColor
                    'animateMotion 'animateTransform 'audio 'canvas 'circle 'clipPath
                    'color-profile 'cursor 'defs 'desc 'discard 'ellipse 'feBlend
                    'feColorMatrix 'feComponentTransfer 'feComposite 'feConvolveMatrix
                    'feDiffuseLighting 'feDisplacementMap 'feDistantLight 'feDropShadow
                    'feFlood 'feFuncA 'feFuncB 'feFuncG 'feFuncR 'feGaussianBlur 'feImage
                    'feMerge 'feMergeNode 'feMorphology 'feOffset 'fePointLight
                    'feSpecularLighting 'feSpotLight 'feTile 'feTurbulence 'filter 'font
                    'font-face 'font-face-format 'font-face-name 'font-face-src 'font-face-uri
                    'foreignObject 'g 'glyph 'glyphRef 'hatch 'hatchpath 'hkern 'iframe 'image
                    'line 'linearGradient 'marker 'mask 'mesh 'meshgradient 'meshpatch
                    'meshrow 'metadata 'missing-glyph 'mpath 'path 'pattern 'polygon 'polyline
                    'radialGradient 'rect 'script 'set 'solidcolor 'stop 'style 'svg 'switch
                    'symbol 'text 'textPath 'title 'tref 'tspan 'unknown 'use 'video 'view
                    'vkern})

(def element-macros (clojure.set/union html-elements svg-elements))

(defmacro def-element-macros []
  `(do
     ~@(for [tag element-macros]
         `(m/make-element-macro ~tag))))

(def-element-macros)

(comment
  (macroexpand '(def-element-macros))
  )
