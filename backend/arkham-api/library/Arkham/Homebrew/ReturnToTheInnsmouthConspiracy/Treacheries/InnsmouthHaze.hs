module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.InnsmouthHaze (innsmouthHaze) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype InnsmouthHaze = InnsmouthHaze TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthHaze :: TreacheryCard InnsmouthHaze
innsmouthHaze = treachery InnsmouthHaze Cards.innsmouthHaze

-- | Attached to a location it raises that shroud; next to the agenda, every shroud.
instance HasModifiersFor InnsmouthHaze where
  getModifiersFor (InnsmouthHaze a) = case a.placement of
    AttachedToLocation lid -> modifySelect a (LocationWithId lid) [ShroudModifier 1]
    _ -> modifySelect a Anywhere [ShroudModifier 1]

instance HasAbilities InnsmouthHaze where
  getAbilities (InnsmouthHaze a) =
    [ limitedAbility (MaxPer Cards.innsmouthHaze PerRound 1)
        $ mkAbility a 1
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage InnsmouthHaze where
  runMessage msg t@(InnsmouthHaze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withLocationOf iid $ placeTreachery attrs . AttachedToLocation
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> InnsmouthHaze <$> liftRunMessage msg attrs
