module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheWellspringOfFortune (theWellspringOfFortune, theWellspringOfFortuneEffect) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Helpers.Window (getDrawSource)
import Arkham.Matcher hiding (key)
import Arkham.Window qualified as Window

newtype TheWellspringOfFortune = TheWellspringOfFortune ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWellspringOfFortune :: ScarletKeyCard TheWellspringOfFortune
theWellspringOfFortune = key TheWellspringOfFortune Cards.theWellspringOfFortune

instance HasAbilities TheWellspringOfFortune where
  getAbilities (TheWellspringOfFortune a) = case a.bearer of
    InvestigatorTarget iid ->
      [restricted a 1 (NotScenario "88001" <> youExist (InvestigatorWithId iid)) $ FastAbility Free]
    _ -> []

instance RunMessage TheWellspringOfFortune where
  runMessage msg k@(TheWellspringOfFortune attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[88045]" Null) k
    CampaignSpecific "shift[88045]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          createCardEffect Cards.theWellspringOfFortune (effectInt 2) (attrs.ability 1) attrs
          withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          createCardEffect Cards.theWellspringOfFortune (effectInt 1) (attrs.ability 1) attrs
          withInvestigatorBearer attrs (`flipOver` attrs)
      pure k
    _ -> TheWellspringOfFortune <$> liftRunMessage msg attrs

newtype TheWellspringOfFortuneEffect = TheWellspringOfFortuneEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWellspringOfFortuneEffect :: EffectArgs -> TheWellspringOfFortuneEffect
theWellspringOfFortuneEffect = cardEffect TheWellspringOfFortuneEffect Cards.theWellspringOfFortune

instance HasAbilities TheWellspringOfFortuneEffect where
  getAbilities (TheWellspringOfFortuneEffect a) =
    [restricted a 1 DuringAnySkillTest $ SilentForcedAbility $ WouldRevealChaosTokens #when Anyone]

instance RunMessage TheWellspringOfFortuneEffect where
  runMessage msg e@(TheWellspringOfFortuneEffect attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      getSkillTestInvestigator >>= traverse_ \iid -> do
        checkWhen $ Window.WouldRevealChaosTokens drawSource iid
        case attrs.metaInt of
          Nothing -> error "TheWellspringOfFortuneEffect missing metaInt"
          Just n ->
            push
              $ ReplaceCurrentDraw drawSource iid
              $ Choose (toSource attrs) n ResolveChoice [Undecided Draw, Undecided Draw] [] Nothing
      disableReturn e
    EndRound -> disableReturn e
    _ -> TheWellspringOfFortuneEffect <$> liftRunMessage msg attrs
