;; Brian's lenovo x230 laptop

(pcase system-name
  ("brian-x230"     
    (message "setting customizations for brian-x230")
    (custom-set-variables '(eclim-eclipse-dirs '("/opt/eclipse")))
    (custom-set-variables '(eclim-executable  "/opt/eclipse/eclim")))
  ("caruso-laptop" 
   (message "setting customizations for caruso-laptop")
   (custom-set-variables '(eclim-eclipse-dirs '("/opt/eclipse4.3-kepler")))
   (custom-set-variables '(eclim-executable  "/opt/eclipse4.3-kepler/eclim")))
)
