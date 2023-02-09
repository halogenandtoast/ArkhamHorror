module Arkham.Treachery.Cards.TerrorFromBeyond
  ( TerrorFromBeyond(..)
  , terrorFromBeyond
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

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
          count (== toCardCode attrs) (historyTreacheriesDrawn phaseHistory) > 1
      iidsWithAssets <- forToSnd iids
        $ fieldMap
          InvestigatorHand
          (map toCardId . filter (`cardMatch` AssetCard))
      iidsWithEvents <- forToSnd iids
        $ fieldMap
          InvestigatorHand
          (map toCardId . filter (`cardMatch` EventCard))
      iidsWithSkills <- forToSnd iids
        $ fieldMap
          InvestigatorHand
          (map toCardId . filter (`cardMatch` SkillCard))
      push $ chooseN
        iid
        (if secondCopy then 2 else 1)
        [ Label
          "Assets"
          [ DiscardCard iid' (toSource attrs) aid
          | (iid', assets) <- iidsWithAssets
          , aid <- assets
          ]
        , Label
          "Events"
          [ DiscardCard iid' (toSource attrs) eid
          | (iid', events) <- iidsWithEvents
          , eid <- events
          ]
        , Label
          "Skills"
          [ DiscardCard iid' (toSource attrs) sid
          | (iid', skills) <- iidsWithSkills
          , sid <- skills
          ]
        ]
      pure t
    _ -> TerrorFromBeyond <$> runMessage msg attrs
