module Arkham.Homebrew.AgesUnwound.Acts.FightingTheTide (fightingTheTide) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Agenda (getDoomOnAgenda)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype FightingTheTide = FightingTheTide ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightingTheTide :: ActCard FightingTheTide
fightingTheTide = act (2, A) FightingTheTide Cards.fightingTheTide Nothing

{- | "[action]: __Move.__ Test [willpower] (5). This test gets -1 difficulty for
each doom on the current agenda. If you succeed, move to The Timestream or to
any [[Adrift]] location. If you fail, move to a random other [[Adrift]]
location." / "__Objective__ - If each undefeated investigator has resigned,
advance."
-}
instance HasAbilities FightingTheTide where
  getAbilities (FightingTheTide a) =
    extend
      a
      [ mkAbility a 1 $ ActionAbility #move Nothing (ActionCost 1)
      , onlyOnce
          $ restricted a 2 (notExists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage FightingTheTide where
  runMessage msg a@(FightingTheTide attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      doom <- getDoomOnAgenda
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed $ max 0 (5 - doom))
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      destinations <-
        select $ oneOf [LocationWithTrait Adrift, locationIs Locations.theTimestream]
      chooseTargetM iid destinations $ moveTo (attrs.ability 1) iid
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveToRandomOtherAdrift (attrs.ability 1) iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    -- "Home. ->R1."
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> FightingTheTide <$> liftRunMessage msg attrs
