module Arkham.Asset.Assets.AlchemicalDistillation (alchemicalDistillation) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Customization
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers hiding (abilityModifier)
import Arkham.Helpers.SkillTest.Target
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype AlchemicalDistillation = AlchemicalDistillation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalDistillation :: AssetCard AlchemicalDistillation
alchemicalDistillation = asset AlchemicalDistillation Cards.alchemicalDistillation

instance HasModifiersFor AlchemicalDistillation where
  getModifiersFor (AlchemicalDistillation a) =
    modifySelfWhen a (a `hasCustomization` Refined) [AdditionalStartingUses 2]

instance HasAbilities AlchemicalDistillation where
  getAbilities (AlchemicalDistillation a) =
    [ skillTestAbility
        $ restricted a 1 ControlsThis
        $ actionAbilityWithCost (assetUseCost a Supply 1)
    ]

instance RunMessage AlchemicalDistillation where
  runMessage msg a@(AlchemicalDistillation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      if attrs `hasCustomization` Empowered
        then chooseOneM iid do
          (cardI18n $ labeled' "alchemicalDistillation.empowerIncreaseDifficultyBy2") do
            abilityModifier
              (AbilityRef (toSource attrs) 1)
              (attrs.ability 1)
              attrs
              (MetaModifier $ object ["empowered" .= True])
            do_ msg
          (cardI18n $ labeled' "alchemicalDistillation.doNotEmpower") $ do_ msg
        else do_ msg
      pure a
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      choices <- select (affectsOthers $ colocatedWith iid)
      chooseOrRunOneM iid $ targets choices $ handleTarget iid (attrs.ability 1)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      empowered <- getMetaMaybe False attrs "empowered"
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid' #intellect (Fixed $ if empowered then 3 else 1)
      pure a
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n -> do
      do_ msg
      when (n >= 2 && attrs `hasCustomization` Perfected) do
        getSkillTestTarget >>= \case
          Just (InvestigatorTarget iid) -> chooseOneM iid do
            (cardI18n $ labeled' "alchemicalDistillation.resolveASecondOption") $ do_ msg
            (cardI18n $ labeled' "alchemicalDistillation.doNotResolveASecondOption") nothing
          _ -> pure ()
      pure a
    Do msg'@(PassedThisSkillTest _ (isAbilitySource attrs 1 -> True)) -> do
      empowered <- getMetaMaybe False attrs "empowered"
      let modify = if empowered then (+ 1) else id
      getSkillTestTarget >>= \case
        Just (InvestigatorTarget iid) -> do
          mDrawCards <- runMaybeT do
            cards <- MaybeT $ Msg.drawCardsIfCan iid (attrs.ability 1) (modify 2)
            pure $ Label (withI18n $ countVar (modify 2) $ ikey' "label.drawCards") [cards]
          mGainResources <- runMaybeT do
            resources <- MaybeT $ Msg.gainResourcesIfCan iid (attrs.ability 1) (modify 2)
            pure $ Label (withI18n $ countVar (modify 2) $ ikey' "label.gainResources") [resources]
          mMendingDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` MendingDistillate
            guardM $ lift $ iid <=~> HealableInvestigator (attrs.ability 1) #damage Anyone
            pure
              $ Label
                (withI18n $ countVar (modify 2) $ ikey' "label.healDamage")
                [HealDamage (toTarget iid) (attrs.ability 1) (modify 2)]
          mCalmingDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` CalmingDistillate
            guardM $ lift $ iid <=~> HealableInvestigator (attrs.ability 1) #horror Anyone
            pure
              $ Label
                (withI18n $ countVar (modify 2) $ ikey' "label.healHorror")
                [HealHorror (toTarget iid) (attrs.ability 1) (modify 2)]
          mEnlighteningDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` EnlighteningDistillate
            guardM $ lift $ selectAny $ assetControlledBy iid <> oneOf (AssetCanHaveUses <$> [Charge, Secret])
            pure
              $ Label
                (cardI18n $ countVar (modify 1) $ ikey' "label.alchemicalDistillation.placeChargeOrSecret")
                [ForInvestigator iid (DoStep 1 msg')]
          mQuickeningDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` QuickeningDistillate
            guardM $ lift $ notNull <$> getAccessibleLocations iid attrs
            pure
              $ Label
                (cardI18n $ countVar (modify 2) $ ikey' "label.alchemicalDistillation.moveUpTo")
                [ForInvestigator iid (DoStep 2 msg')]
          let choices =
                catMaybes
                  [ mDrawCards
                  , mGainResources
                  , mMendingDistillate
                  , mCalmingDistillate
                  , mEnlighteningDistillate
                  , mQuickeningDistillate
                  ]
          when (notNull choices) $ chooseOne iid choices
        _ -> pure ()
      pure a
    ForInvestigator iid (DoStep 1 (PassedThisSkillTest _ (isAbilitySource attrs 1 -> True))) -> do
      chargeAssets <- select $ assetControlledBy iid <> AssetCanHaveUses Charge
      secretAssets <- select $ assetControlledBy iid <> AssetCanHaveUses Secret
      player <- getPlayer iid
      empowered <- getMetaMaybe False attrs "empowered"
      let modify = if empowered then (+ 1) else id

      chooseOne iid $ flip map (nub $ chargeAssets <> secretAssets) \x -> do
        targetLabel
          x
          [ Msg.chooseOrRunOne player
              $ [ Label
                    (withI18n $ countVar (modify 1) $ tokenVar Charge $ ikey' "label.placeToken")
                    [PlaceTokens (attrs.ability 1) (toTarget x) Charge (modify 1)]
                | x `elem` chargeAssets
                ]
              <> [ Label
                     (withI18n $ countVar (modify 1) $ tokenVar Secret $ ikey' "label.placeToken")
                     [PlaceTokens (attrs.ability 1) (toTarget x) Secret (modify 1)]
                 | x `elem` secretAssets
                 ]
          ]
      pure a
    ForInvestigator _ (DoStep 2 (PassedThisSkillTest _ (isAbilitySource attrs 1 -> True))) -> do
      empowered <- getMetaMaybe False attrs "empowered"
      doStep (if empowered then 3 else 2) msg
      pure a
    DoStep
      n
      msg'@(ForInvestigator iid (DoStep 2 (PassedThisSkillTest _ (isAbilitySource attrs 1 -> True)))) | n > 0 -> do
        locations <- getAccessibleLocations iid attrs
        chooseOneM iid do
          (cardI18n $ labeled' "alchemicalDistillation.doneMoving") nothing
          targets locations \lid -> moveTo attrs iid lid >> doStep (n - 1) msg'
        pure a
    _ -> AlchemicalDistillation <$> liftRunMessage msg attrs
