module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheMothBurnedByTheFlame (
  theMothBurnedByTheFlame,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  guestEntryToll,
  guestParleySuccess,
  scenarioI18n,
 )
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (SkillTestAutomaticallySucceeds))

newtype TheMothBurnedByTheFlame = TheMothBurnedByTheFlame AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMothBurnedByTheFlame :: AssetCard TheMothBurnedByTheFlame
theMothBurnedByTheFlame =
  assetWith TheMothBurnedByTheFlame Cards.theMothBurnedByTheFlame ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities TheMothBurnedByTheFlame where
  -- "Forced - After you enter The Moth's location: ..." and
  -- "[action]: Parley. Test [willpower] (4)."
  getAbilities (TheMothBurnedByTheFlame a) =
    [ mkAbility a 1 $ forced $ Enters #after You (locationWithAsset a.id)
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TheMothBurnedByTheFlame where
  runMessage msg a@(TheMothBurnedByTheFlame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guestEntryToll attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      -- "When you initiate this test, you may place 1 doom on any other card to
      -- make this test automatically succeed."
      others <- getOtherCards attrs
      chooseOneM iid $ scenarioI18n $ scope "theMothBurnedByTheFlame" do
        unless (null others) $ labeled "placeDoomOnAnotherCard" do
          chooseTargetM iid others \target -> placeDoom (attrs.ability 2) target 1
          skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
        unscoped skip_
      parley sid iid (attrs.ability 2) attrs #willpower (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      guestParleySuccess attrs 2 iid
      pure a
    _ -> TheMothBurnedByTheFlame <$> liftRunMessage msg attrs

-- | Every card in play that can hold doom, other than The Moth herself.
getOtherCards :: HasGame m => AssetAttrs -> m [Target]
getOtherCards attrs = do
  assets <- selectTargets (not_ (be attrs) :: AssetMatcher)
  enemies <- selectTargets AnyEnemy
  locations <- selectTargets Anywhere
  treacheries <- selectTargets AnyTreachery
  acts <- selectTargets AnyAct
  agendas <- selectTargets AnyAgenda
  pure $ assets <> enemies <> locations <> treacheries <> acts <> agendas
