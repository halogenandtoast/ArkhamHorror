module Arkham.Event.Cards.ManipulateDestiny2 (manipulateDestiny2, ManipulateDestiny2 (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy

newtype ManipulateDestiny2 = ManipulateDestiny2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manipulateDestiny2 :: EventCard ManipulateDestiny2
manipulateDestiny2 = event ManipulateDestiny2 Cards.manipulateDestiny2

componentLabel :: Targetable target => GameTokenType -> target -> [Message] -> UI Message
componentLabel component (toTarget -> target) = case target of
  InvestigatorTarget iid' -> ComponentLabel (InvestigatorComponent iid' component)
  AssetTarget aid -> ComponentLabel (AssetComponent aid component)
  _ -> error "unhandled target"

damageComponentLabel :: Targetable target => target -> Source -> UI Message
damageComponentLabel (toTarget -> thing) source = componentLabel DamageToken thing [HealDamage thing source 2]

getInvestigatorChoices :: HasGame m => InvestigatorId -> Source -> m [UI Message]
getInvestigatorChoices iid source = do
  damageInvestigators <- select $ HealableInvestigator source #damage $ colocatedWith iid
  pure [damageComponentLabel i source | i <- damageInvestigators]

healableAsset :: Sourceable source => source -> DamageType -> LocationMatcher -> AssetMatcher
healableAsset (toSource -> source) hType loc = HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)

getAssetChoices :: HasGame m => InvestigatorId -> Source -> m [UI Message]
getAssetChoices iid source = do
  damageAssets <- select $ healableAsset source #damage (locationWithInvestigator iid)
  pure [damageComponentLabel asset' source | asset' <- damageAssets]

instance RunMessage ManipulateDestiny2 where
  runMessage msg e@(ManipulateDestiny2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      pushAll
        [ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
        , ResetChaosTokens (toSource attrs)
        ]
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      if any ((`elem` [#curse, #autofail, #bless, #eldersign]) . (.face)) tokens
        then do
          enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
          investigatorChoices <- getInvestigatorChoices iid (toSource attrs)
          assetChoices <- getAssetChoices iid (toSource attrs)
          let allChoices = investigatorChoices <> assetChoices
          let canDamage = any ((`elem` [#curse, #autofail]) . (.face)) tokens && notNull enemies
          let canHeal = any ((`elem` [#bless, #eldersign]) . (.face)) tokens && notNull allChoices

          when (canDamage || canHeal) do
            player <- getPlayer iid
            push
              $ chooseOrRunOneAtATime player
              $ [ Label
                  "Deal 2 damage to an enemy at your location"
                  [chooseOne player [targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 2] | enemy <- enemies]]
                | canDamage
                ]
              <> [ Label
                  "Heal 2 damage from an investigator or Ally asset at your location"
                  [chooseOne player allChoices]
                 | canHeal
                 ]
        else
          push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside

      pure e
    _ -> ManipulateDestiny2 <$> runMessage msg attrs
