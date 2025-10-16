module Arkham.Investigator.Cards.AmandaSharpe (amandaSharpe) where

import Arkham.Ability
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Skill.Cards qualified as Skills

newtype AmandaSharpe = AmandaSharpe InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

amandaSharpe :: InvestigatorCard AmandaSharpe
amandaSharpe =
  investigator AmandaSharpe Cards.amandaSharpe
    $ Stats {health = 7, sanity = 7, willpower = 2, intellect = 2, combat = 2, agility = 2}

instance HasAbilities AmandaSharpe where
  getAbilities (AmandaSharpe attrs) =
    [ self_ attrs 1 $ forced $ PhaseBegins #when #investigation
    , self_ attrs 2 $ forced $ InitiatedSkillTest #at You #any #any #any
    ]

instance HasChaosTokenValue AmandaSharpe where
  getChaosTokenValue iid ElderSign (AmandaSharpe attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AmandaSharpe where
  runMessage msg i@(AmandaSharpe attrs) = runQueueT $ case msg of
    SetupInvestigator iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure . AmandaSharpe $ attrs' & setMeta @(Maybe CardId) Nothing
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards attrs.id attrs 1
      let mCard =
            preview _PlayerCard
              =<< (\cardId -> find ((== cardId) . toCardId) (investigatorCardsUnderneath attrs))
              =<< toResult attrs.meta
      for_ mCard \card -> do
        obtainCard card
        push $ AddToDiscard iid card
      doStep 1 msg
      pure $ AmandaSharpe $ attrs & setMeta @(Maybe CardId) Nothing
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      hand <- field InvestigatorHand iid
      case find (`cardMatch` cardIs Skills.whispersFromTheDeep) hand of
        Nothing -> when (notNull hand) do
          chooseOrRunOneM iid $ targets hand $ handleTarget iid (attrs.ability 1)
        Just whispersFromTheDeep -> do
          chooseOneM iid do
            abilityLabeled iid (mkAbility (proxied whispersFromTheDeep.id attrs) 1 (forced AnyWindow)) do
              handleTarget iid (attrs.ability 1) whispersFromTheDeep.id
      pure i
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      placeUnderneath iid (only card)
      pure $ AmandaSharpe $ attrs & setMeta @(Maybe CardId) (Just cid)
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let meta = toResult @(Maybe CardId) attrs.meta
      withSkillTest \sid -> do
        for_ meta \cardId -> do
          card <- getCard cardId
          committable <- getIsCommittable iid card
          when committable do
            -- because we force it to be committed, we do not pay additional costs
            skillTestModifiers sid attrs cardId [MustBeCommitted, NoAdditionalCosts, LeaveCardWhereItIs]
            commitCard iid card
      pure i
    ElderSignEffect iid | attrs `is` iid -> do
      withSkillTest \sid -> do
        for_ (toResult @(Maybe CardId) attrs.meta) \cardId -> do
          chooseOneM iid do
            labeled "Double skill icons" $ skillTestModifier sid (toSource attrs) cardId DoubleSkillIcons
            labeled "Do not double skill icons" nothing
      pure i
    _ -> AmandaSharpe <$> liftRunMessage msg attrs
