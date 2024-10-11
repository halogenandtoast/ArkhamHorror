module Arkham.Investigator.Cards.JimCulverParallel (jimCulverParallel, JimCulverParallel (..)) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Game.Helpers (getModifiedChaosTokenFace)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Import.Lifted qualified as Msg (Message (RevealChaosToken))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Token

newtype JimCulverParallel = JimCulverParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

jimCulverParallel :: InvestigatorCard JimCulverParallel
jimCulverParallel =
  investigator JimCulverParallel Cards.jimCulverParallel
    $ Stats {health = 7, sanity = 8, willpower = 4, intellect = 3, combat = 3, agility = 2}

instance HasAbilities JimCulverParallel where
  getAbilities (JimCulverParallel a) =
    [ playerLimit PerRound
        $ restricted
          a
          1
          (Self <> exists (AssetControlledBy You <> AssetNotAtUsesX <> AssetWithUseType Charge))
        $ freeReaction
          (RevealChaosTokensDuringSkillTest #after You (YourSkillTest AnySkillTest) $ oneOf [#skull, #curse])
    ]

instance HasChaosTokenValue JimCulverParallel where
  getChaosTokenValue iid ElderSign (JimCulverParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JimCulverParallel where
  runMessage msg i@(JimCulverParallel attrs) = runQueueT $ case msg of
    When (Msg.RevealChaosToken _ iid token) | iid == attrs.id -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) do
        chooseOneM iid do
          labeled "Resolve as {elderSign}" nothing
          labeled "Resolve as {skull}" $ chaosTokenEffect attrs token $ ChaosTokenFaceModifier [Skull]
          labeled "Resolve as {curse}" $ chaosTokenEffect attrs token $ ChaosTokenFaceModifier [CurseToken]
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetNotAtUsesX <> AssetWithUseType Charge
      chooseTargetM iid assets $ \a -> push $ AddUses (attrs.ability 1) a Charge 1
      pure i
    _ -> JimCulverParallel <$> liftRunMessage msg attrs
