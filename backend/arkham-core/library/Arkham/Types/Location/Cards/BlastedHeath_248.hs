module Arkham.Types.Location.Cards.BlastedHeath_248
  ( blastedHeath_248
  , BlastedHeath_248(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (blastedHeath_248)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype BlastedHeath_248 = BlastedHeath_248 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_248 :: LocationId -> BlastedHeath_248
blastedHeath_248 = BlastedHeath_248 . baseAttrs
  Cards.blastedHeath_248
  4
  (Static 3)
  Square
  [Circle, Hourglass]

instance HasModifiersFor env BlastedHeath_248 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility Free)
    & (abilityLimitL .~ GroupLimit PerGame 1)

instance ActionRunner env => HasActions env BlastedHeath_248 where
  getActions iid FastPlayerWindow (BlastedHeath_248 attrs) =
    withBaseActions iid FastPlayerWindow attrs $ do
      investigatorsWithClues <- notNull <$> locationInvestigatorsWithClues attrs
      anyAbominations <- notNull <$> locationEnemiesWithTrait attrs Abomination
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | investigatorsWithClues && anyAbominations
        ]
  getActions iid window (BlastedHeath_248 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BlastedHeath_248 where
  runMessage msg l@(BlastedHeath_248 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      investigatorWithCluePairs <- filter ((> 0) . snd) <$> traverse
        (traverseToSnd (fmap unClueCount . getCount))
        (setToList $ locationInvestigators attrs)
      abominations <-
        map EnemyTarget <$> locationEnemiesWithTrait attrs Abomination
      when
        (null investigatorWithCluePairs || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      let
        totalClues = sum $ map snd investigatorWithCluePairs
        investigators = map fst investigatorWithCluePairs
        placeClueOnAbomination = chooseOne
          iid
          [ TargetLabel target [SpendClues 1 investigators, PlaceClues target 1]
          | target <- abominations
          ]

      l <$ unshiftMessages
        ([placeClueOnAbomination]
        <> [ chooseOne
               iid
               [ Label "Spend a second clue" [placeClueOnAbomination]
               , Label "Do not spend a second clue" []
               ]
           | totalClues > 1
           ]
        )
    _ -> BlastedHeath_248 <$> runMessage msg attrs
