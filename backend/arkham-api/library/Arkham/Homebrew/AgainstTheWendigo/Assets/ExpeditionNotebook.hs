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
      perform during this round."

      Split the way the Guides' own Wendigo trigger is: a treachery appears by
      being revealed, an enemy by entering play. The draw window alone never saw
      Bernard Epstein, who is put into play by a story card rather than drawn. -}
      controlled a 2 (exists $ InvestigatorAt YourLocation)
        $ triggered
          ( oneOf
              [ DrawCard #after (InvestigatorAt YourLocation) (basic $ CardWithTrait Wendigo <> #treachery) AnyDeck
              , EnemyEntersPlay #after (EnemyWithTrait Wendigo <> EnemyAt YourLocation)
              ]
          )
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
      nextSkillTestModifier iid (attrs.ability 2) iid (AnySkillValue 1)
      pure $ ExpeditionNotebook $ setMeta True attrs
    {- "If you fail this next test, discard Expedition Notebook."

    The test is whichever one the modifier above lands on, so it is not this
    ability's own and its source says nothing about it. The meta flag is the
    only thing that knows the notebook is riding a test, and either outcome
    clears it. -}
    FailedThisSkillTest iid _
      | armed
      , attrs.controller == Just iid -> do
          toDiscard (attrs.ability 2) attrs
          pure $ ExpeditionNotebook $ setMeta False attrs
    PassedThisSkillTest iid _
      | armed
      , attrs.controller == Just iid ->
          pure $ ExpeditionNotebook $ setMeta False attrs
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      selectForMaybeM
        (InvestigatorAt (locationWithInvestigator iid) <> NotInvestigator (InvestigatorWithId iid))
        $ \other -> takeControlOfAsset other attrs.id
      pure a
    _ -> ExpeditionNotebook <$> liftRunMessage msg attrs
   where
    armed = toResultDefault False attrs.meta
