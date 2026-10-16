{- | Act 2, Essential Materials.

The investigators gather the formula's components onto the first three cards of
the arrangement (the gathering ability is shared with Adapted Formula, see
"Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.Gather"). Advancing puts
Hyde in the body with three horror on him and wipes the agenda clean.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.EssentialMaterials (
  essentialMaterials,
) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.Gather
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher hiding (DuringTurn)

newtype EssentialMaterials = EssentialMaterials ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

essentialMaterials :: ActCard EssentialMaterials
essentialMaterials = act (2, A) EssentialMaterials Cards.essentialMaterials Nothing

instance HasModifiersFor EssentialMaterials where
  getModifiersFor (EssentialMaterials a) = gatherDifficulty a 1

{- | "__Objective__ - If there are at least 2 resources on the first card, 1
resource on the second card, and 1 resource on the third card in the
arrangement, you may advance."
-}
instance HasAbilities EssentialMaterials where
  getAbilities = actAbilities \a ->
    [ gatherAbility "essentialMaterials.gather" a 1
    , scenarioI18n
        $ withI18nTooltip "essentialMaterials.objective"
        $ restricted a 2 (materialsGathered <> DuringTurn Anyone)
        $ Objective
        $ FastAbility Free
    , gatherClueAbility a 3 1
    ]

materialsGathered :: Criterion
materialsGathered =
  mconcat
    [ exists $ arrangementWith 1 (atLeast 2)
    , exists $ arrangementWith 2 (atLeast 1)
    , exists $ arrangementWith 3 (atLeast 1)
    ]

instance RunMessage EssentialMaterials where
  runMessage msg a@(EssentialMaterials attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginGatherTest (attrs.ability 1) attrs iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      askCluesToSpend "essentialMaterials" attrs iid
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> spent) (isTarget attrs -> True) -> do
      spendCluesForSkillValue (attrs.ability 1) iid spent
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      placeGatheredResource (attrs.ability 1) iid
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      whenM (selectAny henryJekyll) $ flipToHyde attrs
      -- The top-up reads the horror on the face in play, so it waits for the flip.
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      placeHorrorOnHydeUpTo attrs 3
      selectEach AnyAgenda $ removeAllDoom attrs
      advanceActDeck attrs
      pure a
    _ -> EssentialMaterials <$> liftRunMessage msg attrs
