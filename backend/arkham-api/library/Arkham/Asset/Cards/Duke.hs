module Arkham.Asset.Cards.Duke (Duke (..), duke) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.SkillTest
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Duke = Duke AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duke :: AssetCard Duke
duke = allyWith Duke Cards.duke (2, 3) noSlots

instance HasModifiersFor Duke where
  getModifiersFor (InvestigatorTarget iid) (Duke a) | controlledBy a iid = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    case (mAction, mSource) of
      (Just Action.Fight, Just source) | isSource a source -> do
        modified a [BaseSkillOf #combat 4, DamageDealt 1]
      (Just Action.Investigate, Just source) | isSource a source -> do
        modified a [BaseSkillOf #intellect 4]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Duke where
  getAbilities (Duke a) =
    [ fightAbility a 1 (exhaust a) ControlsThis
    , delayAdditionalCostsWhen (youExist $ InvestigatorCanMoveTo (a.ability 2) Anywhere)
        $ investigateAbility a 2 (exhaust a) ControlsThis
    ]

instance RunMessage Duke where
  runMessage msg a@(Duke attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 windows' _ -> do
      let source = attrs.ability 2
      lid <- getJustLocation iid
      accessibleLocationIds <- getAccessibleLocations iid source
      investigateAbilities <-
        field LocationAbilities lid >>= filterM \ab ->
          (abilityIs ab #investigate &&)
            <$> getCanPerformAbility iid (defaultWindows iid) (decreaseAbilityActionCost ab 1)
      let
        investigateActions =
          map
            ( (\f -> f windows' [] [])
                . AbilityLabel iid
                . ( \a' ->
                      a'
                        { abilityDoesNotProvokeAttacksOfOpportunity = True
                        , abilitySource = ProxySource a'.source source
                        }
                  )
                . (`decreaseAbilityActionCost` 1)
            )
            investigateAbilities
      chooseOne iid
        $ investigateActions
        <> [ targetLabel lid' [Move $ move attrs iid lid', DoStep 1 msg]
           | lid' <- accessibleLocationIds
           ]
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      lid <- getJustLocation iid
      selectOne (AbilityIs (LocationSource lid) 101) >>= traverse_ \ab ->
        whenM (getCanPerformAbility iid (defaultWindows iid) (decreaseAbilityActionCost ab 1)) do
          sid <- getRandom
          investigate' <- mkInvestigateLocation sid iid attrs lid
          push $ CheckAdditionalActionCosts iid (toTarget lid) #investigate [toMessage investigate']
      pure a
    UseThisAbility iid (ProxySource (LocationSource lid) (isAbilitySource attrs 2 -> True)) 101 -> do
      selectOne (AbilityIs (LocationSource lid) 101) >>= traverse_ \ab ->
        whenM (getCanPerformAbility iid (defaultWindows iid) $ decreaseAbilityActionCost ab 1) do
          sid <- getRandom
          pushM $ mkInvestigateLocation sid iid attrs lid
      pure a
    _ -> Duke <$> liftRunMessage msg attrs
