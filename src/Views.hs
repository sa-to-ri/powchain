{-# LANGUAGE OverloadedStrings #-}

module Views
  ( mainView
  , chainView
  , transactionView
  , peerView
  ) where  

import           Text.Blaze.Html.Renderer.Text    (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5                 (Html, (!))
import qualified Text.Blaze.Html5.Attributes   as A
                                                       
import           Data.Text.Lazy                   (Text)
import           Data.Set                         (Set)
import           Data.Foldable                    (traverse_)

import           Chain                            (Chain, getBlocks)
import           Block                            (Block, blockId, transactions, previousBlock, proof)
import           Transaction                      (Transaction, sender, recipient, amount)
import           HTTP                             (RequestResult)

mainView :: Chain -> Text
mainView chain = renderHtml $ do
  H.html $ do
    H.head $ do
      H.link ! A.href "./static/powchain.css" ! A.rel "stylesheet"
    H.body $ do
      H.div ! A.class_ "tabs" $ do
        H.input ! A.type_ "radio" ! A.id "tab1" ! A.name "tab-control" ! A.checked ""
        H.input ! A.type_ "radio" ! A.id "tab2" ! A.name "tab-control"
        H.input ! A.type_ "radio" ! A.id "tab3" ! A.name "tab-control"

        H.ul $ do
          H.li ! A.title "Chain" $ do
            H.label ! A.for "tab1" ! A.role "button" ! A.onclick "loadTab(0)" $ do
              H.br
              H.span $ do
                H.toHtml ("Chain" :: Text)
          H.li ! A.title "Transactions" $ do
            H.label ! A.for "tab2" ! A.role "button" ! A.onclick "loadTab(1)" $ do
              H.br
              H.span $ do
                H.toHtml ("Transactions" :: Text)
          H.li ! A.title "Peers" $ do
            H.label ! A.for "tab3" ! A.role "button" ! A.onclick "loadTab(2)" $ do
              H.br
              H.span $ do
                H.toHtml ("Peers" :: Text)

        H.div ! A.class_ "slider" $ do
          H.div ! A.class_ "indicator" $ do
            H.toHtml ("" :: Text)

        H.div ! A.id "contents" ! A.class_ "contents" $ do
          chainTable chain

      H.script ! A.src "./static/powchain.js" $ do
        H.toHtml ("" :: Text)

chainView :: Chain -> Text
chainView chain = renderHtml $ do
  chainTable chain

chainTable :: Chain -> Html
chainTable chain =
  H.table ! A.class_ "table styled-table" $ do
    H.col ! A.style "width: 88px"
    H.col ! A.style "width: 83px"
    H.col ! A.style "width: auto"
    H.col ! A.style "width: auto"
    H.col ! A.style "width: auto"
    H.col ! A.style "width: 88px"
    
    H.thead $ do
      H.tr $ do
        H.th $ do
          H.button ! A.class_ "mine-button" ! A.onclick "startMining()" $ do
            H.toHtml ("" :: Text)
  
          H.button ! A.class_ "refresh-button" ! A.onclick "loadTab(0)" $ do
            H.toHtml ("" :: Text)
        H.th $ do
          H.toHtml ("Block" :: Text)
        H.th ! A.colspan "3" $ do
          H.toHtml ("Previous Block" :: Text)
        H.th $ do
          H.toHtml ("Proof" :: Text)
    
    H.tbody $ do
      traverse_ nestedBlockTable (getBlocks chain)

nestedBlockTable :: Block -> Html
nestedBlockTable block = do
  let rowId = show $ blockId block

  H.tr ! A.class_ "level1" $ do
    H.td $ do
      H.button ! A.class_ "expand-button" ! A.onclick (H.toValue ("toggleCollapse(" <> rowId <> ")")) $ do
        H.toHtml ("" :: Text)
    H.td $ do
      H.toHtml $ rowId 
    H.td ! A.colspan "3" $ do
      H.toHtml $ previousBlock block
    H.td $ do
      H.toHtml $ proof block

  H.tr ! A.class_ (H.toValue ("level2 l2_" <> rowId)) ! A.style "display: none" $ do
    H.td ! A.colspan "2" $ do
      H.toHtml ("" :: Text)
    H.td $ do
      H.toHtml ("Sender" :: Text)
    H.td $ do
      H.toHtml ("Recipient" :: Text)
    H.td$ do
      H.toHtml ("Amount" :: Text)
    H.td$ do
      H.toHtml ("" :: Text)

    traverse_ (nestedTransactionRow rowId) (transactions block)

nestedTransactionRow :: String -> Transaction -> Html
nestedTransactionRow rowId transaction =
  H.tr ! A.class_ (H.toValue ("level3 l3_" <> rowId)) ! A.style "display: none" $ do
    H.td ! A.colspan "2" $ do
      H.toHtml ("" :: Text)
    H.td $ do
      H.toHtml $ sender transaction
    H.td $ do
      H.toHtml $ recipient transaction
    H.td $ do
      H.toHtml $ amount transaction
    H.td$ do
      H.toHtml ("" :: Text)

transactionView :: Set Transaction -> Text
transactionView transactions = renderHtml $ do
  H.table ! A.class_ "table styled-table" $ do
    H.col ! A.style "width: 140px"
    H.col ! A.style "width: auto"
    H.col ! A.style "width: auto"
    H.col ! A.style "width: 115px"
    
    H.thead $ do
      H.tr $ do
        H.th $ do
          H.button ! A.class_ "refresh-button" ! A.onclick "loadTab(1)" $ do
            H.toHtml ("" :: Text)
        H.th $ do
          H.toHtml ("Sender" :: Text)
        H.th $ do
          H.toHtml ("Recipient" :: Text)
        H.th $ do
          H.toHtml ("Amount" :: Text)
        
    H.tbody $ do
      H.tr $ do
        H.td $ do
          H.button ! A.class_ "submit-button" ! A.onclick "postTransaction()" $ do
            H.toHtml ("Add transaction" :: Text)
        H.td $ do
          H.input ! A.type_ "text" ! A.id "tx_sender" ! A.class_ "tx_input"
        H.td $ do
          H.input ! A.type_ "text" ! A.id "tx_recipient" ! A.class_ "tx_input"
        H.td $ do
          H.input ! A.type_ "text" ! A.id "tx_amount" ! A.class_ "tx_input"

        traverse_ transactionRow transactions 
    
transactionRow :: Transaction -> Html
transactionRow transaction =
  H.tr $ do
    H.td $ do
      H.toHtml ("" :: Text)
    H.td $ do
      H.toHtml $ sender transaction
    H.td $ do
      H.toHtml $ recipient transaction
    H.td $ do
      H.toHtml . show $ amount transaction

peerView :: Set (Text, RequestResult) -> Text
peerView peers = renderHtml $ do
  H.table ! A.class_ "table styled-table" $ do
    H.col ! A.style "width: 140px"
    H.col ! A.style "width: auto"
    H.col ! A.style "width: 115px"
    
    H.thead $ do
      H.tr $ do
        H.th $ do
          H.button ! A.class_ "refresh-button" ! A.onclick "loadTab(2)" $ do
            H.toHtml ("" :: Text)
        H.th $ do
          H.toHtml ("Peer" :: Text)
        H.th $ do
          H.toHtml ("Status" :: Text)
        
    H.tbody $ do
      H.tr $ do
        H.td $ do
          H.button ! A.class_ "submit-button" ! A.onclick "postPeer()" $ do
            H.toHtml ("Add peer" :: Text)
        H.td $ do
          H.input ! A.type_ "text" ! A.id "peer" ! A.class_ "tx_input"
        H.td $ do
          H.toHtml ("" :: Text)
        
        traverse_ peerRow peers
    
peerRow :: (Text, RequestResult) -> Html
peerRow (peer, health) =
  H.tr $ do
    H.td $ do
      H.button ! A.class_ "remove-button" ! A.onclick (H.toValue $ "deletePeer('" <> peer <> "')") $ do
        H.toHtml ("" :: Text)
    H.td $ do
      H.toHtml peer
    H.td $ do
      H.button ! A.class_ (H.toValue $ show health <> "-button") $ do
        H.toHtml ("" :: Text)

