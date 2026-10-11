module Arkham.Homebrew.AgainstTheWendigo.Assets.ExpeditionNotebook (expeditionNotebook) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (handOverGuideAbility)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wendigo)
import Arkham.Matcher

newtype ExpeditionNotebook = ExpeditionNotebook AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionNotebook :: AssetCard ExpeditionNotebook
expeditionNotebook = asset ExpeditionNotebook Cards.expeditionNotebook

instance HasAbilities ExpeditionNotebook where
  getAbilities (ExpeditionNotebook a) =
    [ {- "Forced - When you take control of Expedition Notebook: Test {willpower}
      (3). If you fail, take 1 direct horror."

      @#after@, not @#when@: the notebook is attached to its location until
      someone takes it, so at the when-window there is no controller yet. -}
      mkAbility a 1 $ forced $ TookControlOfAsset #after You (be a)
    , {- "{reaction} If an investigator in your location reveals or puts into play
      a Wendigo card: you get +1 on your skill value for the next test you
      perform during this round." -}
      controlled a 2 (exists $ InvestigatorAt YourLocation)
        $ triggered
          (DrawCard #after (InvestigatorAt YourLocation) (basic $ CardWithTrait Wendigo) AnyDeck)
          mempty
    , handOverGuideAbility a 3
    ]

instance RunMessage ExpeditionNotebook where
  runMessage msg a@(ExpeditionNotebook attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      directHorror iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      roundModifier (attrs.ability 2) iid (AnySkillValue 1)
      pure a
    -- "If you fail this next test, discard Expedition Notebook."
    FailedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (attrs.ability 2) attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      selectForMaybeM
        (InvestigatorAt (locationWithInvestigator iid) <> NotInvestigator (InvestigatorWithId iid))
        $ \other -> takeControlOfAsset other attrs.id
      pure a
    _ -> ExpeditionNotebook <$> liftRunMessage msg attrs
