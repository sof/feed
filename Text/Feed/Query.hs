--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Query
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------


module Text.Feed.Query
       ( Text.Feed.Query.feedItems -- :: Feed.Feed -> [Feed.Item]

       , FeedGetter               -- type _ a = Feed -> a
       , getFeedTitle             -- :: FeedGetter String
       , getFeedAuthor            -- :: FeedGetter String
       , getFeedHome              -- :: FeedGetter URLString
       , getFeedHTML              -- :: FeedGetter URLString
       , getFeedDescription       -- :: FeedGetter String
       , getFeedPubDate           -- :: FeedGetter DateString
       , getFeedLastUpdate        -- :: FeedGetter (Maybe String)
       , getFeedDate              -- :: FeedGetter DateString
       , getFeedLogoLink          -- :: FeedGetter URLString
       , getFeedLanguage          -- :: FeedGetter String
       , getFeedCategories        -- :: FeedGetter [(String, Maybe String)]
       , getFeedGenerator         -- :: FeedGetter String
       , getFeedItems             -- :: FeedGetter [Item]

       , ItemGetter               -- type _ a = Item -> Maybe a
       , getItemTitle             -- :: ItemGetter (String)
       , getItemLink              -- :: ItemGetter (String)
       , getItemPublishDate       -- :: ItemGetter (DateString)
       , getItemDate              -- :: ItemGetter (DateString)
       , getItemAuthor            -- :: ItemGetter (String)
       , getItemCommentLink       -- :: ItemGetter (URLString)
       , getItemEnclosure         -- :: ItemGetter (String,Maybe String,Integer)
       , getItemFeedLink          -- :: ItemGetter (URLString)
       , getItemId                -- :: ItemGetter (Bool,String)
       , getItemCategories        -- :: ItemGetter [String]
       , getItemRights            -- :: ItemGetter String
       , getItemSummary           -- :: ItemGetter String
       , getItemDescription       -- :: ItemGetter String (synonym of previous.)

       ) where

import Text.Feed.Types as Feed

import Text.RSS.Syntax  as RSS
import Text.Atom.Feed   as Atom
import Text.RSS1.Syntax as RSS1
import Text.XML.Light as XML

import Text.DublinCore.Types

import Data.List
import Data.Maybe
--import Debug.Trace

feedItems :: Feed.Feed -> [Feed.Item]
feedItems fe =
  case fe of
    AtomFeed f -> map Feed.AtomItem (Atom.feedEntries f)
    RSSFeed f  -> map Feed.RSSItem  (RSS.rssItems $ RSS.rssChannel f)
    RSS1Feed f -> map Feed.RSS1Item (RSS1.feedItems f)
     -- ToDo: look for 'entry' elements if 'items' are missing..
    XMLFeed f  -> map Feed.XMLItem $ XML.findElements (XML.unqual "item") f


getFeedItems :: Feed.Feed -> [Feed.Item]
getFeedItems = Text.Feed.Query.feedItems

type FeedGetter a = Feed.Feed -> Maybe a

getFeedAuthor       :: Feed.Feed -> (Maybe String)
getFeedAuthor ft =
  case ft of
    Feed.AtomFeed f -> case Atom.feedAuthors f of { [] -> Nothing; (x:_) -> Just (Atom.personName x)}
    Feed.RSSFeed  f -> RSS.rssEditor (RSS.rssChannel f)
    Feed.RSS1Feed f -> 
      case filter isAuthor $ RSS1.channelDC (RSS1.feedChannel f) of
       (dci:_) -> Just (dcText dci)
       _ -> Nothing
    Feed.XMLFeed f  ->
      case findElement (unqual "channel") f of
        Just e1 -> (fmap XML.strContent $ findElement (unqual "editor") e1)
        Nothing -> Nothing
 where
  isAuthor dc  = dcElt dc == DC_Creator

getFeedTitle       :: Feed.Feed -> String
getFeedTitle ft =
  case ft of
    Feed.AtomFeed f -> (contentToStr $ Atom.feedTitle f)
    Feed.RSSFeed  f -> RSS.rssTitle (RSS.rssChannel f)
    Feed.RSS1Feed f -> RSS1.channelTitle (RSS1.feedChannel f)
    Feed.XMLFeed  f ->
      case findElement (unqual "channel") f of
        Just e1 -> fromMaybe "" (fmap XML.strContent $ findElement (unqual "title") e1)
        Nothing -> ""

getFeedHome        :: FeedGetter URLString
getFeedHome ft =
  case ft of
    Feed.AtomFeed f ->
      case filter isSelf (Atom.feedLinks f) of
        (l:_) -> Just (Atom.linkHref l)
        _ -> Nothing
    Feed.RSSFeed  f -> Just (RSS.rssLink (RSS.rssChannel f))
    Feed.RSS1Feed f -> Just (RSS1.channelURI (RSS1.feedChannel f))
    Feed.XMLFeed  f ->
      case findElement (unqual "channel") f of
        Just e1 -> fmap XML.strContent $ findElement (unqual "link") e1
        Nothing -> Nothing
 where
  isSelf lr = toStr (Atom.linkRel lr) == "self"

getFeedHTML        :: FeedGetter URLString
getFeedHTML ft =
  case ft of
    Feed.AtomFeed f -> 
      case filter isSelf (Atom.feedLinks f) of
        (l:_) -> Just (Atom.linkHref l)
        _ -> Nothing
    Feed.RSSFeed  f -> Just (RSS.rssLink (RSS.rssChannel f))
    Feed.RSS1Feed f -> Just (RSS1.channelURI (RSS1.feedChannel f))
    Feed.XMLFeed  f -> 
      case findElement (unqual "channel") f of
        Just e1 -> fmap XML.strContent $ findElement (unqual "link") e1
        Nothing -> Nothing
 where 
  isSelf lr = toStr (Atom.linkRel lr) == "alternate" && isHTMLType (linkType lr)

  isHTMLType (Just str) = "lmth" `isPrefixOf` (reverse str)
  isHTMLType _ = True -- if none given, assume html.

getFeedDescription :: FeedGetter String
getFeedDescription ft =
  case ft of
    Feed.AtomFeed f -> fmap contentToStr (Atom.feedSubtitle f)
    Feed.RSSFeed  f -> Just $ RSS.rssDescription (RSS.rssChannel f)
    Feed.RSS1Feed f -> Just (RSS1.channelDesc (RSS1.feedChannel f))
    Feed.XMLFeed  f -> 
      case findElement (unqual "channel") f of
        Just e1 -> fmap XML.strContent $ findElement (unqual "description") e1
        Nothing -> Nothing

getFeedPubDate     :: FeedGetter DateString
getFeedPubDate ft =
  case ft of
    Feed.AtomFeed f -> Just $ Atom.feedUpdated f
    Feed.RSSFeed  f -> RSS.rssPubDate (RSS.rssChannel f)
    Feed.RSS1Feed f -> 
      case filter isDate (RSS1.channelDC $ RSS1.feedChannel f) of
        (l:_) -> Just (dcText l)
        _ -> Nothing
    Feed.XMLFeed  f -> 
      case findElement (unqual "channel") f of
        Just e1 -> fmap XML.strContent $ findElement (unqual "pubDate") e1
        Nothing -> Nothing
 where
  isDate dc  = dcElt dc == DC_Date

getFeedLastUpdate  :: FeedGetter (String)
getFeedLastUpdate ft =
  case ft of
    Feed.AtomFeed f -> Just $ Atom.feedUpdated f
    Feed.RSSFeed  f -> RSS.rssPubDate (RSS.rssChannel f)
    Feed.RSS1Feed f -> 
      case filter isDate (RSS1.channelDC $ RSS1.feedChannel f) of
        (l:_) -> Just (dcText l)
        _ -> Nothing
    Feed.XMLFeed  f -> 
      case findElement (unqual "channel") f of
        Just e1 -> fmap XML.strContent $ findElement (unqual "pubDate") e1
        Nothing -> Nothing
 where
  isDate dc  = dcElt dc == DC_Date

getFeedDate        :: FeedGetter DateString
getFeedDate ft = getFeedPubDate ft

getFeedLogoLink    :: FeedGetter URLString
getFeedLogoLink ft =
  case ft of
    Feed.AtomFeed f -> Atom.feedLogo f
    Feed.RSSFeed  f -> fmap RSS.rssImageURL (RSS.rssImage $ RSS.rssChannel f)
    Feed.RSS1Feed f -> (fmap RSS1.imageURI $ RSS1.feedImage f)
    Feed.XMLFeed  f -> do
       ch <- findElement (unqual "channel") f
       e1 <- findElement (unqual "image") ch
       v  <- findElement (unqual "url") e1
       return (XML.strContent v)

getFeedLanguage    :: FeedGetter String
getFeedLanguage ft =
  case ft of
    Feed.AtomFeed f -> 
       lookupAttr (unqual "lang"){qPrefix=Just "xml"} (Atom.feedAttrs f)
    Feed.RSSFeed  f -> RSS.rssLanguage (RSS.rssChannel f)
    Feed.RSS1Feed f -> 
       case filter isLang (RSS1.channelDC $ RSS1.feedChannel f) of
         (l:_) -> Just (dcText l)
         _ -> Nothing
    Feed.XMLFeed  f -> do
       ch <- findElement (unqual "channel") f
       e1 <- findElement (unqual "language") ch
       return (XML.strContent e1)
 where
  isLang dc  = dcElt dc == DC_Language


getFeedCategories  :: Feed.Feed -> [(String, Maybe String)]
getFeedCategories ft =
  case ft of
    Feed.AtomFeed f -> map (\ c -> (Atom.catTerm c, Atom.catScheme c)) (Atom.feedCategories f)
    Feed.RSSFeed  f -> map (\ c -> (RSS.rssCategoryValue c, RSS.rssCategoryDomain c)) (RSS.rssCategories (RSS.rssChannel f))
    Feed.RSS1Feed f -> 
       case filter isCat (RSS1.channelDC $ RSS1.feedChannel f) of
         ls -> map (\ l -> (dcText l,Nothing)) ls
    Feed.XMLFeed  f -> 
       case fromMaybe [] $ fmap (XML.findElements (XML.unqual "category")) (findElement (unqual "channel") f) of
         ls -> map (\ l -> (fromMaybe "" (fmap XML.strContent $ findElement (unqual "term") l), findAttr (unqual "domain") l)) ls
 where
  isCat dc  = dcElt dc == DC_Subject

getFeedGenerator   :: FeedGetter String
getFeedGenerator ft =
  case ft of
    Feed.AtomFeed f -> do
      gen <- Atom.feedGenerator f
      Atom.genURI gen
    Feed.RSSFeed  f -> RSS.rssGenerator (RSS.rssChannel f)
    Feed.RSS1Feed f -> 
        case filter isSource (RSS1.channelDC (RSS1.feedChannel f)) of
          (l:_) -> Just (dcText l)
          _ -> Nothing
    Feed.XMLFeed  f -> do
       ch <- findElement (unqual "channel") f
       e1 <- findElement (unqual "generator") ch
       return (XML.strContent e1)
 where
        isSource dc = dcElt dc == DC_Source

type ItemGetter a = Feed.Item -> Maybe a

getItemTitle :: ItemGetter String
getItemTitle it = 
  case it of
    Feed.AtomItem i -> Just (contentToStr $ Atom.entryTitle i)
    Feed.RSSItem i  -> RSS.rssItemTitle i
    Feed.RSS1Item i -> Just (RSS1.itemTitle i)
    Feed.XMLItem e  -> fmap XML.strContent $ findElement (unqual "title") e

getItemLink :: ItemGetter String
getItemLink it =
  case it of
    Feed.AtomItem i -> 
       -- look up the 'alternate' HTML link relation on the entry:
       case filter isSelf $ Atom.entryLinks i of
         (l:_) -> Just (Atom.linkHref l)
         _ -> Nothing
    Feed.RSSItem i  -> RSS.rssItemLink i
    Feed.RSS1Item i -> Just (RSS1.itemLink i)
    Feed.XMLItem i  -> fmap (\ ei -> XML.strContent ei) $ findElement (unqual "link") i
 where
  isSelf lr = toStr (Atom.linkRel lr) == "alternate" && isHTMLType (linkType lr)

  isHTMLType (Just str) = "lmth" `isPrefixOf` (reverse str)
  isHTMLType _ = True -- if none given, assume html.


getItemPublishDate :: ItemGetter DateString
getItemPublishDate it =
  case it of
    Feed.AtomItem i -> Just $ Atom.entryUpdated i
    Feed.RSSItem i  -> RSS.rssItemPubDate i
    Feed.RSS1Item i -> 
      case filter isDate $ RSS1.itemDC i of
       (dci:_) -> Just (dcText dci)
       _ -> Nothing
      -- ToDo: look for it in Atom \/ RSS1 like-content as well if no 'pubDate' element.
    Feed.XMLItem e  -> fmap XML.strContent $ findElement (unqual "pubDate") e
 where
  isDate dc  = dcElt dc == DC_Date

getItemDate :: ItemGetter DateString
getItemDate it = getItemPublishDate it

getItemAuthor      :: ItemGetter String
getItemAuthor it = 
  case it of
    Feed.AtomItem i -> case Atom.entryAuthors i of { [] -> Nothing; (x:_) -> Just (Atom.personName x)}
    Feed.RSSItem i  -> RSS.rssItemAuthor i
    Feed.RSS1Item i -> 
      case filter isAuthor $ RSS1.itemDC i of
       (dci:_) -> Just (dcText dci)
       _ -> Nothing
    Feed.XMLItem e  -> fmap XML.strContent $ findElement (unqual "author") e
 where
  isAuthor dc  = dcElt dc == DC_Creator

getItemCommentLink :: ItemGetter URLString
getItemCommentLink it = 
  case it of
    Feed.AtomItem e -> 
       -- look up the 'replies' HTML link relation on the entry:
       case filter isReplies $ Atom.entryLinks e of
         (l:_) -> Just (Atom.linkHref l)
         _ -> Nothing
    Feed.RSSItem i  -> RSS.rssItemComments i
    Feed.RSS1Item i -> 
       case filter isRel $ RSS1.itemDC i of
         (l:_) -> Just (dcText l)
         _ -> Nothing
    Feed.XMLItem i  -> fmap (\ ei -> XML.strContent ei) $ findElement (unqual "comments") i
 where
  isReplies lr = toStr (Atom.linkRel lr) == "replies"
  isRel dc = dcElt dc == DC_Relation

getItemEnclosure   :: ItemGetter (String, Maybe String, Integer)
getItemEnclosure it = 
  case it of
    Feed.AtomItem e ->
       case filter isEnc $ Atom.entryLinks e of
         (l:_) -> Just (Atom.linkHref l, Atom.linkType l, 
                        case Atom.linkLength l of 
                          Nothing -> 0
                          Just x  -> 
                            case reads x of { [] -> 0; ((v,_):_) -> v })
         _ -> Nothing
    Feed.RSSItem i  ->
       fmap (\ e -> (RSS.rssEnclosureURL e, Just (RSS.rssEnclosureType e), RSS.rssEnclosureLength e)) 
            (RSS.rssItemEnclosure i)
    Feed.RSS1Item i -> 
       case RSS1.itemContent i of
         [] -> Nothing
         (c:_) -> Just (fromMaybe "" (RSS1.contentURI c), RSS1.contentFormat c, 0)
    Feed.XMLItem e  -> fmap toV (findElement (unqual "enclosure") e)
 where
   isEnc lr = toStr (Atom.linkRel lr) == "enclosure"

   toV e = ( fromMaybe "" (fmap XML.strContent (findElement (unqual "url") e))
           , fmap XML.strContent (findElement (unqual "type") e)
           , fromMaybe 0 (fmap (read . XML.strContent) (findElement (unqual "length") e))
           )

getItemFeedLink    :: ItemGetter URLString
getItemFeedLink it = 
  case it of
    Feed.AtomItem e -> 
       case (Atom.entrySource e) of
         Nothing -> Nothing
         Just s  -> Atom.sourceId s
    Feed.RSSItem i ->
       case (RSS.rssItemSource i) of
         Nothing -> Nothing
         Just s  -> Just (RSS.rssSourceURL s)
    Feed.RSS1Item _ -> Nothing
    Feed.XMLItem e -> 
      case findElement (unqual "source") e of
        Nothing -> Nothing
        Just s  -> fmap XML.strContent (findElement (unqual "url") s)

getItemId          :: ItemGetter (Bool,String)
getItemId it = 
  case it of
    Feed.AtomItem e -> Just (True, Atom.entryId e)
    Feed.RSSItem i  -> 
      case RSS.rssItemGuid i of
        Nothing -> Nothing
        Just ig -> Just (fromMaybe True (RSS.rssGuidPermanentURL ig), RSS.rssGuidValue ig)
    Feed.RSS1Item i -> 
      case filter isId (RSS1.itemDC i) of
        (l:_) -> Just (True,dcText l)
        _ -> Nothing
    Feed.XMLItem e -> 
      fmap (\ e1 -> (True,XML.strContent e1)) (findElement (unqual "guid") e)
 where
  isId dc = dcElt dc == DC_Identifier

getItemCategories  :: Feed.Item -> [String]
getItemCategories it = 
  case it of
    Feed.AtomItem i -> map Atom.catTerm $ Atom.entryCategories i
    Feed.RSSItem i  -> map RSS.rssCategoryValue $ RSS.rssItemCategories i
    Feed.RSS1Item i -> concat $ getCats1 i
    Feed.XMLItem i  -> map XML.strContent $ XML.findElements (XML.unqual "category") i
 where
    -- get RSS1 categories; either via DublinCore's subject (or taxonomy topics...not yet.)
   getCats1 i1 = 
     map (words.dcText) $ filter (\ dc -> dcElt dc == DC_Subject) $ RSS1.itemDC i1


getItemRights      :: ItemGetter String
getItemRights it = 
  case it of
    Feed.AtomItem e -> fmap contentToStr $ Atom.entryRights e
    Feed.RSSItem  _ -> Nothing
    Feed.RSS1Item i ->
      case filter isRights (RSS1.itemDC i) of
        (l:_) -> Just (dcText l)
        _ -> Nothing
    Feed.XMLItem _ -> Nothing
 where
  isRights dc = dcElt dc == DC_Rights

getItemSummary      :: ItemGetter String
getItemSummary it = getItemDescription it

getItemDescription :: ItemGetter String
getItemDescription it = 
  case it of
    Feed.AtomItem e -> fmap contentToStr $ Atom.entrySummary e
    Feed.RSSItem  e -> RSS.rssItemDescription e
    Feed.RSS1Item i -> itemDesc i
    Feed.XMLItem _  -> Nothing

 -- strip away
toStr :: Maybe (Either String String) -> String
toStr Nothing = ""
toStr (Just (Left x)) = x
toStr (Just (Right x)) = x

contentToStr :: TextContent -> String
contentToStr x = 
  case x of
    Atom.TextString  s -> s
    Atom.HTMLString  s -> s
    Atom.XHTMLString s -> XML.strContent s
