module Arkham.Types.Location.Cards.BishopsBrook_203
  ( bishopsBrook_203
  , BishopsBrook_203(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BishopsBrook_203 = BishopsBrook_203 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_203 :: BishopsBrook_203
bishopsBrook_203 = BishopsBrook_203 $ baseAttrs
  "02203"
  (Name "Bishop's Brook" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (Static 2)
  Square
  [Plus, Circle, Triangle]
  [Dunwich]

instance HasModifiersFor env BishopsBrook_203 where
  getModifiersFor (InvestigatorSource iid) target (BishopsBrook_203 attrs@LocationAttrs {..})
    | isTarget attrs target
    = pure $ toModifiers
      attrs
      [ Blocked
      | iid `notElem` locationInvestigators && not (null locationInvestigators)
      ]
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability a = base { abilityLimit = GroupLimit PerGame 1 }
  where base = mkAbility (toSource a) 1 (FastAbility Free)

instance ActionRunner env => HasActions env BishopsBrook_203 where
  getActions iid FastPlayerWindow (BishopsBrook_203 attrs@LocationAttrs {..}) =
    withBaseActions iid FastPlayerWindow attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | locationClues == 0 && locationRevealed
      ]
  getActions iid window (BishopsBrook_203 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BishopsBrook_203 where
  runMessage msg (BishopsBrook_203 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      case locationCardsUnderneath attrs of
        (EncounterCard card : rest) -> do
          unshiftMessage (InvestigatorDrewEncounterCard iid card)
          pure $ BishopsBrook_203 $ attrs & cardsUnderneathL .~ rest
        _ -> throwIO $ InvalidState "Not expecting a player card"
    _ -> BishopsBrook_203 <$> runMessage msg attrs
