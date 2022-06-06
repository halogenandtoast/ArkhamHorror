module Arkham.Treachery.Cards.TerrorFromBeyond
  ( TerrorFromBeyond(..)
  , terrorFromBeyond
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.History
import Arkham.Message
import Arkham.Treachery.Attrs

newtype TerrorFromBeyond = TerrorFromBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorFromBeyond :: TreacheryCard TerrorFromBeyond
terrorFromBeyond = treachery TerrorFromBeyond Cards.terrorFromBeyond

instance RunMessage TerrorFromBeyond where
  runMessage msg t@(TerrorFromBeyond attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      iids <- getInvestigatorIds
      phaseHistory <- mconcat <$> traverse (getHistory PhaseHistory) iids
      let
        secondCopy =
          toCardCode attrs `elem` historyTreacheriesDrawn phaseHistory
      iidsWithAssets <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, AssetType))
        iids
      iidsWithEvents <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, EventType))
        iids
      iidsWithSkills <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, SkillType))
        iids
      t <$ push
        (chooseN
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
        )
    _ -> TerrorFromBeyond <$> runMessage msg attrs
