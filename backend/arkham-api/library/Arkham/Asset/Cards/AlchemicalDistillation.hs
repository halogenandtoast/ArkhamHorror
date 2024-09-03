module Arkham.Asset.Cards.AlchemicalDistillation (alchemicalDistillation, AlchemicalDistillation (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Customization
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), getMetaMaybe)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Query (getPlayer)
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Movement

newtype AlchemicalDistillation = AlchemicalDistillation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalDistillation :: AssetCard AlchemicalDistillation
alchemicalDistillation = asset AlchemicalDistillation Cards.alchemicalDistillation

instance HasModifiersFor AlchemicalDistillation where
  getModifiersFor target (AlchemicalDistillation attrs) | attrs `is` target = do
    let refined = attrs `hasCustomization` Refined
    modified attrs [AdditionalStartingUses 2 | refined]
  getModifiersFor _ _ = pure []

instance HasAbilities AlchemicalDistillation where
  getAbilities (AlchemicalDistillation a) =
    [ skillTestAbility
        $ restrictedAbility a 1 ControlsThis
        $ actionAbilityWithCost (assetUseCost a Supply 1)
    ]

instance RunMessage AlchemicalDistillation where
  runMessage msg a@(AlchemicalDistillation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      if attrs `hasCustomization` Empowered
        then
          chooseOne
            iid
            [ Label
                "Empower (increase difficulty by 2)"
                [ Msg.abilityModifier
                    (AbilityRef (toSource attrs) 1)
                    (attrs.ability 1)
                    attrs
                    (MetaModifier $ object ["empowered" .= True])
                , Do msg
                ]
            , Label "Do not empower" [Do msg]
            ]
        else push $ Do msg
      pure a
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      choices <- select (affectsOthers $ colocatedWith iid)
      chooseOrRunOne iid $ targetLabels choices $ only . handleTargetChoice iid (attrs.ability 1)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      empowered <- getMetaMaybe False attrs "empowered"
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid' #intellect (Fixed $ if empowered then 3 else 1)
      pure a
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n -> do
      push $ Do msg
      when (n >= 2 && attrs `hasCustomization` Perfected) do
        getSkillTestTarget >>= \case
          Just (InvestigatorTarget iid) ->
            chooseOne iid [Label "Resolve a second option" [Do msg], Label "Do not resolve a second option" []]
          _ -> pure ()
      pure a
    Do msg'@(PassedThisSkillTest _ (isAbilitySource attrs 1 -> True)) -> do
      empowered <- getMetaMaybe False attrs "empowered"
      let modify = if empowered then (+ 1) else id
      getSkillTestTarget >>= \case
        Just (InvestigatorTarget iid) -> do
          mDrawCards <- runMaybeT do
            drawCards <- MaybeT $ Msg.drawCardsIfCan iid (attrs.ability 1) (modify 2)
            pure $ Label ("Draw " <> tshow (modify 2) <> " cards") [drawCards]
          mGainResources <- runMaybeT do
            gainResources <- MaybeT $ Msg.gainResourcesIfCan iid (attrs.ability 1) (modify 2)
            pure $ Label ("Gain " <> tshow (modify 2) <> " resources") [gainResources]
          mMendingDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` MendingDistillate
            guardM $ lift $ iid <=~> HealableInvestigator (attrs.ability 1) #damage Anyone
            pure
              $ Label
                ("Heal " <> tshow (modify 2) <> " damage")
                [HealDamage (toTarget iid) (attrs.ability 1) (modify 2)]
          mCalmingDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` CalmingDistillate
            guardM $ lift $ iid <=~> HealableInvestigator (attrs.ability 1) #horror Anyone
            pure
              $ Label
                ("Heal " <> tshow (modify 2) <> " horror")
                [HealHorror (toTarget iid) (attrs.ability 1) (modify 2)]
          mEnlighteningDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` EnlighteningDistillate
            guardM $ lift $ selectAny $ assetControlledBy iid <> oneOf (AssetCanHaveUses <$> [Charge, Secret])
            pure
              $ Label
                ( "Place "
                    <> pluralize (modify 1) "charge"
                    <> " or "
                    <> pluralize_ (modify 1) "secret"
                    <> " on an asset you control"
                )
                [ForInvestigator iid (DoStep 1 msg')]
          mQuickeningDistillate <- runMaybeT do
            guard $ attrs `hasCustomization` QuickeningDistillate
            guardM $ lift $ notNull <$> getAccessibleLocations iid attrs
            pure $ Label ("Move up to " <> tshow (modify 2) <> " times") [ForInvestigator iid (DoStep 2 msg')]
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
                  ("Place " <> tshow (modify 1) <> " Charge")
                  [PlaceTokens (attrs.ability 1) (toTarget x) Charge (modify 1)]
                | x `elem` chargeAssets
                ]
              <> [ Label
                  ("Place " <> tshow (modify 1) <> " Secret")
                  [PlaceTokens (attrs.ability 1) (toTarget x) Secret (modify 1)]
                 | x `elem` secretAssets
                 ]
          ]
      pure a
    ForInvestigator _ (DoStep 2 (PassedThisSkillTest _ (isAbilitySource attrs 1 -> True))) -> do
      empowered <- getMetaMaybe False attrs "empowered"
      push $ DoStep (if empowered then 3 else 2) msg
      pure a
    DoStep
      n
      msg'@(ForInvestigator iid (DoStep 2 (PassedThisSkillTest _ (isAbilitySource attrs 1 -> True)))) | n > 0 -> do
        locations <- getAccessibleLocations iid attrs
        chooseOne iid
          $ Label "Done Moving" []
          : [targetLabel lid [Move $ move attrs iid lid, DoStep (n - 1) msg'] | lid <- locations]
        pure a
    _ -> AlchemicalDistillation <$> liftRunMessage msg attrs
