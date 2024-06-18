module Arkham.Treachery.Cards.SerpentsCall (serpentsCall, SerpentsCall (..)) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SerpentsCall = SerpentsCall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsCall :: TreacheryCard SerpentsCall
serpentsCall = treachery SerpentsCall Cards.serpentsCall

instance RunMessage SerpentsCall where
  runMessage msg t@(SerpentsCall attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      let draw = drawEncounterCards iid attrs 2
      isPoisoned <- getIsPoisoned iid
      if isPoisoned
        then push draw
        else do
          poisoned <- getSetAsidePoisoned
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label
                  "Put a set-aside Poisoned weakness into play in your threat area"
                  [CreateWeaknessInThreatArea poisoned iid]
              , Label "Draw the top 2 cards of the encounter deck" [draw]
              ]
      pure t
    _ -> SerpentsCall <$> runMessage msg attrs
