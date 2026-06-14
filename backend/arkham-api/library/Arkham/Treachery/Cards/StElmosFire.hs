module Arkham.Treachery.Cards.StElmosFire (stElmosFire) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StElmosFire = StElmosFire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stElmosFire :: TreacheryCard StElmosFire
stElmosFire = treachery StElmosFire Cards.stElmosFire

instance HasAbilities StElmosFire where
  getAbilities (StElmosFire a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage StElmosFire where
  runMessage msg t@(StElmosFire attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.stElmosFire)
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToLocation lid -> do
          -- Each investigator at this location takes 1 damage.
          investigators <- select $ investigatorAt lid
          for_ investigators \iid -> assignDamage iid (attrs.ability 1) 1
          -- Then attach St. Elmo's Fire to an adjacent location (preferring one
          -- with investigators).
          let adjacentWithoutFire =
                connectedTo (LocationWithId lid)
                  <> LocationWithoutTreachery (treacheryIs Cards.stElmosFire)
          withInvestigators <- select $ adjacentWithoutFire <> LocationWithInvestigator Anyone
          candidates <-
            if notNull withInvestigators
              then pure withInvestigators
              else select adjacentWithoutFire
          -- TODO: in Obsidian Canyons, "adjacent" should also account for the
          -- Summit-deck "open sky" cards and sliding locations, which have no
          -- engine support yet; using grid/connection adjacency for now.
          case candidates of
            [] -> pure ()
            (target : _) -> place attrs (AttachedToLocation target)
        _ -> pure ()
      pure t
    _ -> StElmosFire <$> liftRunMessage msg attrs
