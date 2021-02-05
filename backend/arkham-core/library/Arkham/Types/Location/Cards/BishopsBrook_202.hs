module Arkham.Types.Location.Cards.BishopsBrook_202
  ( bishopsBrook_202
  , BishopsBrook_202(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BishopsBrook_202 = BishopsBrook_202 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_202 :: BishopsBrook_202
bishopsBrook_202 = BishopsBrook_202 $ baseAttrs
  "02202"
  (Name "Bishop's Brook" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (Static 2)
  Square
  [Plus, Circle, Triangle]
  [Dunwich]

instance HasModifiersFor env BishopsBrook_202 where
  getModifiersFor _ (EnemyTarget eid) (BishopsBrook_202 attrs@LocationAttrs {..})
    | eid `elem` locationEnemies
    = pure $ toModifiers attrs [HorrorDealt 1]
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability a = base { abilityLimit = GroupLimit PerGame 1 }
  where base = mkAbility (toSource a) 1 (FastAbility Free)

instance ActionRunner env => HasActions env BishopsBrook_202 where
  getActions iid FastPlayerWindow (BishopsBrook_202 attrs@LocationAttrs {..}) =
    withBaseActions iid FastPlayerWindow attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | locationClues == 0 && locationRevealed
      ]
  getActions iid window (BishopsBrook_202 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BishopsBrook_202 where
  runMessage msg (BishopsBrook_202 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      case locationCardsUnderneath attrs of
        (EncounterCard card : rest) -> do
          unshiftMessage (InvestigatorDrewEncounterCard iid card)
          pure $ BishopsBrook_202 $ attrs & cardsUnderneathL .~ rest
        _ -> throwIO $ InvalidState "Not expecting a player card"
    _ -> BishopsBrook_202 <$> runMessage msg attrs
