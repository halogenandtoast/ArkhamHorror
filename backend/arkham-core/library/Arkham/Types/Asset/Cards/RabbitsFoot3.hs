module Arkham.Types.Asset.Cards.RabbitsFoot3
  ( RabbitsFoot3(..)
  , rabbitsFoot3
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype RabbitsFoot3 = RabbitsFoot3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot3 :: AssetCard RabbitsFoot3
rabbitsFoot3 = accessory RabbitsFoot3 Cards.rabbitsFoot3

instance HasModifiersFor env RabbitsFoot3

ability :: AssetAttrs -> Int -> Ability
ability attrs n =
  (mkAbility (toSource attrs) 1 (LegacyReactionAbility $ ExhaustCost (toTarget attrs))
    )
    { abilityMetadata = Just (IntMetadata n)
    }

instance HasAbilities env RabbitsFoot3 where
  getAbilities iid (AfterFailSkillTest who n) (RabbitsFoot3 a)
    | ownedBy a iid && iid == who = pure [ability a n]
  getAbilities i window (RabbitsFoot3 x) = getAbilities i window x

instance AssetRunner env => RunMessage env RabbitsFoot3 where
  runMessage msg a@(RabbitsFoot3 attrs) = case msg of
    UseCardAbility iid source (Just (IntMetadata x)) 1 _
      | isSource attrs source -> a <$ push
        (SearchTopOfDeck
          iid
          source
          (InvestigatorTarget iid)
          x
          mempty
          (ShuffleBackIn $ DrawFound iid)
        )
    _ -> RabbitsFoot3 <$> runMessage msg attrs
