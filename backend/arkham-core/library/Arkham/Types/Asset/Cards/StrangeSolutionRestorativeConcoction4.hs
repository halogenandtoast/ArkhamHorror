module Arkham.Types.Asset.Cards.StrangeSolutionRestorativeConcoction4
  ( strangeSolutionRestorativeConcoction4
  , StrangeSolutionRestorativeConcoction4(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype StrangeSolutionRestorativeConcoction4 = StrangeSolutionRestorativeConcoction4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionRestorativeConcoction4
  :: AssetCard StrangeSolutionRestorativeConcoction4
strangeSolutionRestorativeConcoction4 = asset
  StrangeSolutionRestorativeConcoction4
  Cards.strangeSolutionRestorativeConcoction4

ability :: AssetAttrs -> Ability
ability a = restrictedAbility
  a
  1
  (InvestigatorExists $ InvestigatorAtYourLocation <> InvestigatorWithDamage)
  (ActionAbility Nothing $ Costs [ActionCost 1, UseCost (toId a) Supply 1])

instance HasActions env StrangeSolutionRestorativeConcoction4 where
  getActions iid NonFast (StrangeSolutionRestorativeConcoction4 attrs)
    | ownedBy attrs iid = pure [ability attrs]
  getActions iid window (StrangeSolutionRestorativeConcoction4 attrs) =
    getActions iid window attrs

instance HasModifiersFor env StrangeSolutionRestorativeConcoction4

instance
  ( HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  , HasQueue env, HasModifiersFor env ()
  )
  => RunMessage env StrangeSolutionRestorativeConcoction4 where
  runMessage msg a@(StrangeSolutionRestorativeConcoction4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      targets <- map InvestigatorTarget
        <$> getSetList @InvestigatorId locationId
      a <$ push
        (chooseOne
          iid
          [ TargetLabel target [HealDamage target 2] | target <- targets ]
        )
    _ -> StrangeSolutionRestorativeConcoction4 <$> runMessage msg attrs
