{- | "{action}: Heal 1 damage and 1 horror from you or an Ally asset you control.
Any investigator at Henry Jekyll's location may activate this ability. (Limit
once per round.)"

The second sentence is what makes this 'OnSameLocation' rather than
'ControlsThis' -- nobody controls him, he stands at a location. The limit says
nothing about the group, and "unless stated otherwise, limits are player
specific" (@mcp/references/grimoire/glossary/limits_and_maximums.md@), so it is
a 'playerLimit'.

Jekyll is also one face of the Jekyll/Hyde card; the flip and the horror that
rides along with it live in the scenario's Helpers.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Assets.HenryJekyll (henryJekyll) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Damage
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers hiding (henryJekyll)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HenryJekyll = HenryJekyll AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

henryJekyll :: AssetCard HenryJekyll
henryJekyll = asset HenryJekyll Cards.henryJekyll

-- | "an Ally asset you control", as the heal sees it.
healableAlly :: Sourceable source => source -> DamageType -> InvestigatorMatcher -> AssetMatcher
healableAlly source kind who =
  HealableAsset (toSource source) kind (#ally <> AssetControlledBy who)

instance HasAbilities HenryJekyll where
  getAbilities (HenryJekyll a) =
    [ scenarioI18n
        $ withI18nTooltip "henryJekyll.heal"
        $ playerLimit PerRound
        $ restricted a 1 (OnSameLocation <> hasSomethingToHeal) actionAbility
    ]
   where
    hasSomethingToHeal =
      oneOf
        [ exists $ oneOf [HealableInvestigator (toSource a) kind You | kind <- [#damage, #horror]]
        , exists $ oneOf [healableAlly a kind You | kind <- [#damage, #horror]]
        ]

instance RunMessage HenryJekyll where
  runMessage msg a@(HenryJekyll attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let who = InvestigatorWithId iid
      healSelf <- orM [canHaveDamageHealed source iid, canHaveHorrorHealed source iid]
      damagedAllies <- select $ healableAlly source #damage who
      horrifiedAllies <- select $ healableAlly source #horror who
      let allies = nub $ damagedAllies <> horrifiedAllies
      chooseOneM iid $ scenarioI18n $ scope "henryJekyll" do
        when healSelf $ labeled "healYou" do
          healDamageIfCan iid source 1
          healHorrorIfCan iid source 1
        unless (null allies) do
          labeled "healAlly" $ chooseTargetM iid allies \aid -> do
            when (aid `elem` damagedAllies) $ healDamage aid source 1
            when (aid `elem` horrifiedAllies) $ healHorror aid source 1
      pure a
    _ -> HenryJekyll <$> liftRunMessage msg attrs
