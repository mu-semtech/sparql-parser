(in-package #:sparql-visjs)

(defparameter *render-tokens* nil "Render tokens in vis.js.  These are duplicate and it's likely simpler without.")

(defun tree-as-visjs-nodes-and-edges (tree)
  "Construct nodes and edges for TREE to be rendered in a Visjs page.

Returns two values, a JSOWN representation of the nodes and a JSOWN
representation of the edges."
  ;; give each match a name
  ;; pick a label and text content for each match
  ;; construct the relationships table
  (let ((match (reasoner-tree-mirror:reasoner-ast-node tree))
        (index 0)
        (node-descriptions)
        (edge-descriptions))
    (labels ((new-id (label)
               (format nil "~A-~A"
                       (as-string label)
                       (incf index)))
             (as-string (term-or-string)
               (typecase term-or-string
                 (string term-or-string)
                 (symbol (symbol-name term-or-string))))
             (html-escape (string)
               "Escapes a string so it's happy to be positioned in JS HTML."
               (cl-ppcre:regex-replace-all ">" (cl-ppcre:regex-replace-all "<" string "&lt;") "&gt;"))
             (determine-color (match)
               (let ((term (match-term match))
                     (expansion (rule-expansion match)))
                 (cond ((sparql-parser:terminalp term)
                        (format nil "rgb(233,233,233)"))
                       ((and (= (length expansion) 2)
                             (not (consp (elt expansion 1))))
                        ;; one element
                        "rgb(200,249,248)")
                       ((and (eq (first expansion) 'ebnf:seq)
                             (notany #'consp expansion))
                        "rgb(120,249,100)")
                       ((and (eq (first expansion) 'ebnf:alt)
                             (notany #'consp expansion))
                        "rgb(201,249,120)")
                       (t (format nil "rgb(234,90,122)")))))
             (rule-expansion (match)
               (when (match-p match)
                 (let ((rule (sparql-generator::find-rule (match-term match))))
                   (and rule (ebnf:rule-expansion rule)))))
             (make-node-title (match tree)
               (format nil "~A~%~A~%~A"
                       (html-escape (sparql-generator::write-valid-match match))
                       (alexandria:if-let ((rule-expansion (rule-expansion match)))
                         (html-escape (princ-to-string rule-expansion))
                         "UNKNOWN")
                       (and tree (reasoner-term-info:print-term-info tree nil))))
             (traverse-match (match tree &optional parent-id)
               (let ((id (new-id (typecase match
                                   (match (match-term match))
                                   (scanned-token (scanned-token-token match))))))
                 (typecase match
                   (match ;; describe the match
                     (push (jsown:new-js
                               ("id" id)
                               ("label" (as-string (match-term match)))
                               ("title" (make-node-title match tree))
                               ("color" (determine-color match);; (jsown:new-js
                                        ;; ("background" (determine-color match))
                                        ;; ("border" "rgb(233,233,233)"))
                                        ))
                             node-descriptions)
                     ;; add reference to parent if parent exists
                     (when parent-id
                       (push (jsown:new-js
                               ("from" parent-id)
                               ("to" id))
                             edge-descriptions))
                     (let ((child-trees (and tree (reasoner-tree-mirror:reasoner-ast-children tree))))
                       (dolist (submatch (match-submatches match))
                         (traverse-match submatch
                                         (find submatch child-trees :key #'reasoner-tree-mirror:reasoner-ast-node)
                                         id))))
                   (scanned-token
                    ;; describe the token
                    ;; add reference to parent if parent exists
                    (when *render-tokens*
                      (let ((token match))
                        (push (jsown:new-js
                                ("id" id)
                                ("label" (as-string (scanned-token-token token)))
                                ("title" (html-escape (scanned-token-effective-string token))))
                              node-descriptions)
                        (push (jsown:new-js
                                ("from" parent-id)
                                ("to" id))
                              edge-descriptions))))))))
      (traverse-match match tree)
      (values (jsown:to-json (reverse node-descriptions))
              (jsown:to-json (reverse edge-descriptions))))))

(defun in-page (nodes edges &optional stream)
  "Dumps NODES and EDGES into a page."
  (format stream "~A~&var nodes=~A~&var edges=~A~&~A"
          "<!DOCTYPE html>
<html data-ember-extension=\"1\"><head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">
  <title>SPARQL in a graph by redpencil.io</title>

  <script type=\"text/javascript\" src=\"https://gateway.ipfs.io/ipfs/QmfLbfycQp4NhZHn1WzSo3vUEEcNQSXpMQpAd4afJK4V4F/vis.js\"></script>
  <link href=\"https://gateway.ipfs.io/ipfs/QmfLbfycQp4NhZHn1WzSo3vUEEcNQSXpMQpAd4afJK4V4F/vis-network.css\" rel=\"stylesheet\" type=\"text/css\">

  <style type=\"text/css\">
   #mynetwork {
     position: absolute;
     left: 1px;
     right: 1px;
     top: 1px;
     bottom: 1px;
     border: 1px solid lightgray;
   }
   div.vis-tooltip { white-space: pre }
  </style>
</head>
<body>
  <div id=\"mynetwork\"><div class=\"vis-network\" style=\"position: relative; overflow: hidden; touch-action: pan-y; -moz-user-select: none; width: 100%; height: 100%;\" tabindex=\"900\"><canvas style=\"position: relative; touch-action: none; -moz-user-select: none; width: 100%; height: 100%;\" width=\"1280\" height=\"800\"></canvas></div></div>

  <script type=\"text/javascript\">
   "
          nodes edges
          "   // create a network
   var container = document.getElementById('mynetwork');
   var data = {
     nodes: nodes,
     edges: edges
   };
   var options = {
     layout: {
       improvedLayout: false,
       randomSeed: 751154,
       /* enabled: false, */
       /* nodespacing: 100, */
       hierarchical: {
         enabled: true,
         sortMethod: \"directed\"
       }
     },
     edges: {
       arrows: { to: true },
       smooth: {
         type: \"continuous\",
         forceDirection: \"none\",
         roundness: 0.1
       }
     },
     \"physics\": {
       \"barnesHut\": {
         \"gravitationalConstant\": -10650,
         \"centralGravity\": 0.7,
         \"springLength\": 50,
         \"springConstant\": 0.02,
         \"damping\": 0.85,
         \"avoidOverlap\": 0.75
       },
       \"adaptiveTimestep\": true,
       \"minVelocity\": 0.75,
       \"maxVelocity\": 100,
       \"timestep\": 0.4
     }
   };
   window.onload = () => {
     var network = new vis.Network(container, data, options);
     window.network = network;
     window.data = data;
   };
   /* --> */
  </script>
  Visualization of a <a href=\"http://www.w3.org/TR/sparql11-query/#sparqlGrammar\">SPARQL 1.1</a> query ast with <a href=\"http://visjs.org\">vis.js</a>. Proudly built by <a href=\"http://redpencil.io\">redpencil.io</a>.
</body>
</html>"))

(defun write-page-for-tree (tree path)
  "Writes MATCH as visible query to PATH html file."
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (multiple-value-bind (nodes edges)
        (tree-as-visjs-nodes-and-edges tree)
      (in-page nodes edges out))))
