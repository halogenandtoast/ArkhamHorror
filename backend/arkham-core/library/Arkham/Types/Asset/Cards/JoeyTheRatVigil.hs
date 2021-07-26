module Arkham.Types.Asset.Cards.JoeyTheRatVigil
  ( joeyTheRatVigil
  , JoeyTheRatVigil(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Trait
import Arkham.Types.Window

newtype JoeyTheRatVigil = JoeyTheRatVigil AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeyTheRatVigil :: AssetCard JoeyTheRatVigil
joeyTheRatVigil = ally JoeyTheRatVigil Cards.joeyTheRatVigil (3, 2)

ability :: AssetAttrs -> Ability
ability a = mkAbility a 1 (FastAbility $ ResourceCost 1)

instance HasActions env JoeyTheRatVigil where
  getActions iid FastPlayerWindow (JoeyTheRatVigil attrs) | ownedBy attrs iid =
    pure [UseAbility iid (ability attrs)]
  getActions _ _ _ = pure []

instance HasModifiersFor env JoeyTheRatVigil

instance
  ( HasActions env ActionType
  , HasList HandCard env InvestigatorId
  , HasId LocationId env InvestigatorId
  , HasId LocationId env EnemyId
  , HasSet InvestigatorId env LocationId
  , HasSet Trait env EnemyId
  , HasSet EnemyId env LocationId
  , HasSet EnemyId env InvestigatorId
  , HasCount ClueCount env LocationId
  , HasCount ResourceCount env InvestigatorId
  , HasCount DoomCount env AssetId
  , HasCount DoomCount env InvestigatorId
  , HasList DiscardedPlayerCard env InvestigatorId
  , HasSet InvestigatorId env ()
  , HasSet AssetId env InvestigatorId
  , HasQueue env, HasModifiersFor env ()
  )
  => RunMessage env JoeyTheRatVigil where
  runMessage msg a@(JoeyTheRatVigil attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      handCards <- map unHandCard <$> getList iid
      let items = filter (member Item . toTraits) handCards
      playableItems <- filterM
        (getIsPlayable iid [DuringTurn You, FastPlayerWindow])
        items
      a <$ push
        (chooseOne
          iid
          [ Run
              [ PayCardCost iid (toCardId item)
              , InitiatePlayCard iid (toCardId item) Nothing False
              ]
          | item <- playableItems
          ]
        )
    _ -> JoeyTheRatVigil <$> runMessage msg attrs
