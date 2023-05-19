module Arkham.Treachery.Cards.Wracked (
  wracked,
  Wracked (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.History
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Wracked = Wracked TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wracked :: TreacheryCard Wracked
wracked = treachery Wracked Cards.wracked

instance HasModifiersFor Wracked where
  getModifiersFor (InvestigatorTarget iid) (Wracked attrs) = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource iid' _ source _) | iid == iid' -> do
        performed <-
          historySkillTestsPerformed <$> getHistory RoundHistory iid
        hasExhaustedWitch <-
          selectAny $
            ExhaustedEnemy
              <> EnemyWithTrait Witch
              <> EnemyAt
                (locationWithInvestigator iid)
        pure $
          toModifiers attrs $
            [AnySkillValue (-1) | performed == 0 && treacheryOnInvestigator iid attrs]
              <> [ SkillTestAutomaticallySucceeds
                 | hasExhaustedWitch && isSource attrs source
                 ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Wracked where
  getAbilities (Wracked a) =
    [ restrictedAbility a 1 OnSameLocation $
        ActionAbility Nothing $
          ActionCost
            1
    ]

instance RunMessage Wracked where
  runMessage msg t@(Wracked attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ RevelationSkillTest iid (toSource attrs) SkillWillpower 3
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
        pure t
    _ -> Wracked <$> runMessage msg attrs
