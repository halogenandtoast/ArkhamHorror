module Arkham.Effect.Effects.WilliamYorick (
  WilliamYorick (..),
  williamYorick,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype WilliamYorick = WilliamYorick EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamYorick :: EffectArgs -> WilliamYorick
williamYorick = WilliamYorick . uncurry4 (baseAttrs "03005")

instance RunMessage WilliamYorick where
  runMessage msg e@(WilliamYorick attrs) = case msg of
    PassedSkillTest _ _ _ SkillTestInitiatorTarget {} _ _ ->
      case effectTarget attrs of
        InvestigatorTarget iid -> do
          modifiers' <- getModifiers (InvestigatorTarget iid)
          unless (CardsCannotLeaveYourDiscardPile `elem` modifiers') $ do
            discards <- field InvestigatorDiscard iid
            when (notNull discards)
              $ push
              $ chooseOne iid
              $ Done "Do not return card to hand"
              : [ targetLabel
                  (toCardId card)
                  [addToHand iid $ PlayerCard card]
                | card <- discards
                ]
          pure e
        _ -> pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> WilliamYorick <$> runMessage msg attrs
