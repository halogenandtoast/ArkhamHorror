module Arkham.Homebrew.CircusExMortis.Locations.FoothillSlope_164 (foothillSlope_164) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype FoothillSlope_164 = FoothillSlope_164 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foothillSlope_164 :: LocationCard FoothillSlope_164
foothillSlope_164 = location FoothillSlope_164 Cards.foothillSlope_164 3 (PerPlayer 1)

instance HasAbilities FoothillSlope_164 where
  getAbilities (FoothillSlope_164 a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (CanMoveToLocation You (a.ability 1) (accessibleFrom ForMovement a)))
      $ freeReaction
      $ EnemyEvaded #after You (at_ $ be a)

instance RunMessage FoothillSlope_164 where
  runMessage msg l@(FoothillSlope_164 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <-
        select
          $ CanMoveToLocation (InvestigatorWithId iid) (attrs.ability 1) (accessibleFrom ForMovement attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure l
    _ -> FoothillSlope_164 <$> liftRunMessage msg attrs
