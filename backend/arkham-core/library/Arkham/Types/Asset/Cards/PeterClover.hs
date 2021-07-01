module Arkham.Types.Asset.Cards.PeterClover
  ( peterClover
  , PeterClover(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Window

newtype PeterClover = PeterClover AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterClover :: AssetCard PeterClover
peterClover = allyWith PeterClover Cards.peterClover (3, 2)
  $ (slotsL .~ [])
  . (isStoryL .~ True)

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility $ ExhaustCost (toTarget attrs))

instance
  ( HasSet EnemyId env ([Trait], LocationId)
  , HasId LocationId env InvestigatorId
  )
  => HasActions env PeterClover where
  getActions iid FastPlayerWindow (PeterClover attrs) = do
    lid <- getId @LocationId iid
    criminals <- getSet @EnemyId ([Criminal], lid)
    pure
      [ ActivateCardAbilityAction iid (ability attrs) | not (null criminals) ]
  getActions iid window (PeterClover attrs) = getActions iid window attrs

instance HasModifiersFor env PeterClover where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet EnemyId env ([Trait], LocationId)
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env PeterClover where
  runMessage msg a@(PeterClover attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList ([Criminal], lid)
      a <$ unshiftMessage
        (chooseOne iid [ EnemyEvaded iid eid | eid <- criminals ])
    BeginEnemy | isNothing assetInvestigator ->
      a <$ unshiftMessage (AssetDamage assetId (toSource attrs) 1 0)
    _ -> PeterClover <$> runMessage msg attrs
