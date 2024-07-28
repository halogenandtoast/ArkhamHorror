module Arkham.Asset.Cards.PrismaticSpectaclesLensToTheOtherworld2 (
  prismaticSpectaclesLensToTheOtherworld2,
  prismaticSpectaclesLensToTheOtherworld2Effect,
  PrismaticSpectaclesLensToTheOtherworld2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Investigate
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Modifier

newtype PrismaticSpectaclesLensToTheOtherworld2 = PrismaticSpectaclesLensToTheOtherworld2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticSpectaclesLensToTheOtherworld2 :: AssetCard PrismaticSpectaclesLensToTheOtherworld2
prismaticSpectaclesLensToTheOtherworld2 = asset PrismaticSpectaclesLensToTheOtherworld2 Cards.prismaticSpectaclesLensToTheOtherworld2

instance HasAbilities PrismaticSpectaclesLensToTheOtherworld2 where
  getAbilities (PrismaticSpectaclesLensToTheOtherworld2 x) =
    [ restrictedAbility x 1 ControlsThis $ investigateAction (AddCurseTokenCost 1)
    ]

instance RunMessage PrismaticSpectaclesLensToTheOtherworld2 where
  runMessage msg a@(PrismaticSpectaclesLensToTheOtherworld2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      createCardEffect
        Cards.prismaticSpectaclesLensToTheOtherworld2
        (effectMetaTarget sid)
        (attrs.ability 1)
        iid
      pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    _ -> PrismaticSpectaclesLensToTheOtherworld2 <$> liftRunMessage msg attrs

newtype PrismaticSpectaclesLensToTheOtherworld2Effect
  = PrismaticSpectaclesLensToTheOtherworld2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticSpectaclesLensToTheOtherworld2Effect
  :: EffectArgs -> PrismaticSpectaclesLensToTheOtherworld2Effect
prismaticSpectaclesLensToTheOtherworld2Effect =
  cardEffect
    PrismaticSpectaclesLensToTheOtherworld2Effect
    Cards.prismaticSpectaclesLensToTheOtherworld2

instance RunMessage PrismaticSpectaclesLensToTheOtherworld2Effect where
  runMessage msg e@(PrismaticSpectaclesLensToTheOtherworld2Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token | token.face == #curse -> do
      case attrs.target of
        InvestigatorTarget iid -> void $ runMaybeT do
          aid <- hoistMaybe attrs.source.asset
          liftGuardM $ aid <=~> AssetReady
          sid <- MaybeT getSkillTestId
          metaTarget <- hoistMaybe attrs.metaTarget
          guard $ isTarget sid metaTarget
          lift
            $ chooseOne
              iid
              [ Label
                  "Exhaust Prismatic Spectacles to discover 1 additional clue at your location"
                  [Exhaust (toTarget aid), Msg.skillTestModifier sid attrs.source iid (DiscoveredClues 1)]
              , Label "Do not exaust" []
              ]
        _ -> pure ()
      pure e
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> PrismaticSpectaclesLensToTheOtherworld2Effect <$> liftRunMessage msg attrs
