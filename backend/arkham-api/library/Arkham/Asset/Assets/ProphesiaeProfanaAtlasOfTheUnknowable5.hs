module Arkham.Asset.Assets.ProphesiaeProfanaAtlasOfTheUnknowable5 (
  prophesiaeProfanaAtlasOfTheUnknowable5,
  ProphesiaeProfanaAtlasOfTheUnknowable5 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_, modified_)
import Arkham.History
import Arkham.Matcher
import Arkham.Movement
import Arkham.Taboo

newtype ProphesiaeProfanaAtlasOfTheUnknowable5 = ProphesiaeProfanaAtlasOfTheUnknowable5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prophesiaeProfanaAtlasOfTheUnknowable5 :: AssetCard ProphesiaeProfanaAtlasOfTheUnknowable5
prophesiaeProfanaAtlasOfTheUnknowable5 = asset ProphesiaeProfanaAtlasOfTheUnknowable5 Cards.prophesiaeProfanaAtlasOfTheUnknowable5

instance HasModifiersFor ProphesiaeProfanaAtlasOfTheUnknowable5 where
  getModifiersFor (ProphesiaeProfanaAtlasOfTheUnknowable5 attrs) = do
    let mlocus = maybeResult attrs.meta
    controller <- case attrs.controller of
      Nothing -> pure mempty
      Just iid -> maybeModified_ attrs iid do
        locus <- hoistMaybe mlocus
        liftGuardM $ iid <!=~> InvestigatorAt (LocationWithId locus)
        mTurnInvestigator <- lift $ selectOne TurnInvestigator
        canTaboo <-
          lift
            $ maybe
              (pure False)
              (\iid' -> (== 0) <$> getHistoryField TurnHistory iid' HistoryAttacksOfOpportunity)
              mTurnInvestigator
        pure
          $ [SkillModifier #intellect 1, SkillModifier #agility 1]
          <> if tabooed TabooList20 attrs
            then [IgnoreAttacksOfOpportunity | canTaboo]
            else [MayIgnoreAttacksOfOpportunity]
    locus <- case mlocus of
      Nothing -> pure mempty
      Just lid -> modified_ attrs lid [Locus]
    pure $ controller <> locus

instance HasAbilities ProphesiaeProfanaAtlasOfTheUnknowable5 where
  getAbilities (ProphesiaeProfanaAtlasOfTheUnknowable5 a) =
    let mlocus = maybeResult a.meta
     in restrictedAbility a 1 ControlsThis (freeReaction $ AssetEntersPlay #after (be a))
          : [ controlledAbility
              a
              2
              (exists $ affectsOthers $ InvestigatorCanMoveTo (a.ability 2) (LocationWithId locus))
              actionAbility
            | locus <- toList mlocus
            ]

instance RunMessage ProphesiaeProfanaAtlasOfTheUnknowable5 where
  runMessage msg a@(ProphesiaeProfanaAtlasOfTheUnknowable5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealedLocations <- select RevealedLocation
      chooseOrRunOne
        iid
        [ targetLabel location [HandleTargetChoice iid (attrs.ability 1) (toTarget location)]
        | location <- revealedLocations
        ]
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      pure . ProphesiaeProfanaAtlasOfTheUnknowable5 $ attrs & setMeta (Just lid)
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let locus = toResult attrs.meta
      investigators <-
        select $ affectsOthers $ InvestigatorCanMoveTo (attrs.ability 2) (LocationWithId locus)
      chooseOrRunOne
        iid
        [ targetLabel investigator [Move $ move (attrs.ability 2) investigator locus]
        | investigator <- investigators
        ]
      pure a
    _ -> ProphesiaeProfanaAtlasOfTheUnknowable5 <$> liftRunMessage msg attrs
