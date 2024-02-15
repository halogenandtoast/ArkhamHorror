module Arkham.Treachery.Cards.WhispersOfHypnos (
  whispersOfHypnos,
  whispersOfHypnosEffect,
  WhispersOfHypnos (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WhispersOfHypnos = WhispersOfHypnos TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersOfHypnos :: TreacheryCard WhispersOfHypnos
whispersOfHypnos = treachery WhispersOfHypnos Cards.whispersOfHypnos

instance RunMessage WhispersOfHypnos where
  runMessage msg t@(WhispersOfHypnos attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      player <- getPlayer iid
      effects <- select $ effectFrom Cards.whispersOfHypnos
      usedSkills <- forMaybeM effects $ \effect -> do
        meta <- field EffectMeta effect
        pure $ case meta of
          Just (EffectMetaSkill sType) -> Just sType
          _ -> Nothing

      let skills = filter (`notElem` usedSkills) [#willpower, #intellect, #combat, #agility]
      when (notNull skills) $ do
        push
          $ chooseOrRunOne
            player
            [ SkillLabel
              sType
              [createCardEffect Cards.whispersOfHypnos (Just $ EffectMetaSkill sType) attrs attrs]
            | sType <- skills
            ]
      pure t
    _ -> WhispersOfHypnos <$> runMessage msg attrs

newtype WhispersOfHypnosEffect = WhispersOfHypnosEffect EffectAttrs
  deriving anyclass (IsEffect, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersOfHypnosEffect :: EffectArgs -> WhispersOfHypnosEffect
whispersOfHypnosEffect = cardEffect WhispersOfHypnosEffect Cards.whispersOfHypnos

instance HasModifiersFor WhispersOfHypnosEffect where
  getModifiersFor (InvestigatorTarget _) (WhispersOfHypnosEffect attrs) = do
    case attrs.meta of
      Just (EffectMetaSkill sType) -> pure $ toModifiers attrs [SkillModifier sType (-2)]
      _ -> error "invalid meta"
  getModifiersFor _ _ = pure []

instance RunMessage WhispersOfHypnosEffect where
  runMessage msg e@(WhispersOfHypnosEffect attrs) = case msg of
    EndRound -> do
      push $ disable attrs
      pure e
    _ -> WhispersOfHypnosEffect <$> runMessage msg attrs
