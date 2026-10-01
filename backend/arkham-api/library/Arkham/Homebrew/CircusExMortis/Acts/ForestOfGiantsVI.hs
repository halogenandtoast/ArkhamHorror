module Arkham.Homebrew.CircusExMortis.Acts.ForestOfGiantsVI (
  forestOfGiantsVI,
  forestOfGiantsAbilities,
  forestOfGiantsModifiers,
  flipPathForwardBesideRowOf,
  theCultEnMasseArrives,
) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card.CardDef (CardDef)
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers (getRowIndex)
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathLabelRow)
import Arkham.Matcher
import Arkham.Placement (Placement (AsSelfLocation))
import Arkham.Projection
import Arkham.Story.Types (Field (StoryFlipped, StoryPlacement))

{- | All three Forest of Giants variants print the same front, so the ability list,
its gate, and its handler live here and v.II / v.III import them. Only the back
differs: which set-aside copy of The Cult En Masse spawns at Ritual Clearing.
-}
newtype ForestOfGiantsVI = ForestOfGiantsVI ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forestOfGiantsVI :: ActCard ForestOfGiantsVI
forestOfGiantsVI = act (1, A) ForestOfGiantsVI Cards.forestOfGiantsVI Nothing

{- | "Investigators spend X[per_investigator] clues as a group, where X is 1 less
than the number of locations in your row." X is read off the acting
investigator's own row, so it has to be a calculated cost rather than a
'GroupClueCost' with a fixed 'GameValue'; the clue pool is unrestricted
(@Anywhere@) because the card says "as a group" without naming a location.
-}
pathForwardCost :: Cost
pathForwardCost =
  CalculatedGroupClueCost
    ( MultiplyCalculation
        (SubtractCalculation (CountLocations (LocationInRowOf YourLocation)) (Fixed 1))
        (GameValueCalculation $ PerPlayer 1)
    )
    Anywhere

forestOfGiantsAbilities :: ActAttrs -> [Ability]
forestOfGiantsAbilities x =
  [ mkAbility x 1 $ freeTrigger pathForwardCost
  , mkAbility x 2
      $ Objective
      $ forced
      $ Enters #after Anyone (locationIs Locations.ritualClearing)
  ]

{- | Every facedown Path Forward, paired with the row it sits beside. The cards are
in play at no location: each sits in a grid cell of its own named @path\<row\>@, which
is where the row comes from — the same read
'Arkham.Homebrew.CircusExMortis.Stories.PathForward.besideRow' does from the other side.
-}
getFacedownPathsForward :: HasGame m => m [(StoryId, Int)]
getFacedownPathsForward = do
  facedown <- filterM (field StoryFlipped) =<< select (StoryWithTitle "Path Forward")
  catMaybes <$> for facedown \sid -> do
    field StoryPlacement sid <&> \case
      AsSelfLocation label | Just y <- pathLabelRow label -> Just (sid, y)
      _ -> Nothing

{- | The ability can only ever flip a facedown Path Forward, so it is withheld from
investigators whose row has none beside it: a row of one is dealt no Path
Forward at all, and a flipped one cannot be flipped again.
-}
forestOfGiantsModifiers :: HasModifiersM m => ActAttrs -> m ()
forestOfGiantsModifiers a = when (onSide A a) do
  rows <- map snd <$> getFacedownPathsForward
  modifySelect
    a
    (not_ $ InvestigatorAt $ mapOneOf LocationInRow rows)
    [CannotTriggerAbilityMatching $ AbilityIs (toSource a) 1]

-- | "Flip the facedown Path Forward card beside your row faceup."
flipPathForwardBesideRowOf :: ReverseQueue m => ActAttrs -> InvestigatorId -> m ()
flipPathForwardBesideRowOf attrs iid = do
  row <- getRowIndex =<< getJustLocation iid
  paths <- getFacedownPathsForward
  for_ (take 1 [sid | (sid, r) <- paths, Just r == row]) (flipOverBy iid (attrs.ability 1))

-- | Back: the variant's copy of The Cult En Masse spawns at Ritual Clearing.
theCultEnMasseArrives :: ReverseQueue m => ActAttrs -> CardDef -> m ()
theCultEnMasseArrives attrs def = do
  createSetAsideEnemy_ def (locationIs Locations.ritualClearing)
  advanceActDeck attrs

instance HasAbilities ForestOfGiantsVI where
  getAbilities = actAbilities forestOfGiantsAbilities

instance HasModifiersFor ForestOfGiantsVI where
  getModifiersFor (ForestOfGiantsVI a) = forestOfGiantsModifiers a

instance RunMessage ForestOfGiantsVI where
  runMessage msg a@(ForestOfGiantsVI attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipPathForwardBesideRowOf attrs iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      theCultEnMasseArrives attrs Enemies.theCultEnMasseLeaderlessFanaticism
      pure a
    _ -> ForestOfGiantsVI <$> liftRunMessage msg attrs
