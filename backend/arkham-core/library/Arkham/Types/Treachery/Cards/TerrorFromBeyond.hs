module Arkham.Types.Treachery.Cards.TerrorFromBeyond
  ( TerrorFromBeyond(..)
  , terrorFromBeyond
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.TreacheryId
import Arkham.Types.Target
import Arkham.Types.Card.Id
import Arkham.Types.Helpers
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TerrorFromBeyond = TerrorFromBeyond TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorFromBeyond :: TreacheryId -> a -> TerrorFromBeyond
terrorFromBeyond uuid _ = TerrorFromBeyond $ baseAttrs uuid "02101"

instance HasModifiersFor env TerrorFromBeyond where
  getModifiersFor = noModifiersFor

instance HasActions env TerrorFromBeyond where
  getActions i window (TerrorFromBeyond attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env TerrorFromBeyond where
  runMessage msg t@(TerrorFromBeyond attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      iids <- getSetList ()
      phaseHistory <- getPhaseHistory =<< ask
      let
        secondCopy =
          count
              (\case
                DrewTreachery _ card | getCardCode card == "02101" -> True
                _ -> False
              )
              phaseHistory
            >= 2
      iidsWithAssets <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, AssetType))
        iids
      iidsWithEvents <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, EventType))
        iids
      iidsWithSkills <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, SkillType))
        iids
      t <$ unshiftMessages
        [ chooseN
          iid
          (if secondCopy then 2 else 1)
          [ Label
            "Assets"
            [ Run [ DiscardCard iid' aid | aid <- assets ]
            | (iid', assets) <- iidsWithAssets
            ]
          , Label
            "Events"
            [ Run [ DiscardCard iid' eid | eid <- events ]
            | (iid', events) <- iidsWithEvents
            ]
          , Label
            "Skills"
            [ Run [ DiscardCard iid' sid | sid <- skills ]
            | (iid', skills) <- iidsWithSkills
            ]
          ]
        , Discard $ TreacheryTarget (treacheryId attrs)
        ]
    _ -> TerrorFromBeyond <$> runMessage msg attrs
