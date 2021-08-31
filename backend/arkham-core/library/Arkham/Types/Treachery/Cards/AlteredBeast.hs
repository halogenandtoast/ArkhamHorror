module Arkham.Types.Treachery.Cards.AlteredBeast
  ( AlteredBeast(..)
  , alteredBeast
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AlteredBeast = AlteredBeast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alteredBeast :: TreacheryCard AlteredBeast
alteredBeast = treachery AlteredBeast Cards.alteredBeast

instance HasAbilities env AlteredBeast where
  getAbilities _ _ (AlteredBeast x) = pure $ case treacheryAttachedTarget x of
    Just (EnemyTarget eid) ->
      [ mkAbility x 1 $ ForcedAbility $ OrWindowMatcher
          [ EnemyEnters Timing.When YourLocation $ EnemyWithId eid
          , Enters Timing.When You (LocationWithEnemy $ EnemyWithId eid)
          ]
      ]
    _ -> error "Altered Beast must be attached to an enemy"

instance TreacheryRunner env => RunMessage env AlteredBeast where
  runMessage msg t@(AlteredBeast attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      abominations <- selectListMap EnemyTarget $ EnemyWithTrait Abomination
      t <$ case abominations of
        [] -> pushAll [Surge iid source, Discard $ toTarget attrs]
        xs -> push
          (chooseOrRunOne
            iid
            [ TargetLabel x [AttachTreachery treacheryId x, HealAllDamage x]
            | x <- xs
            ]
          )
    UseCardAbility iid source _ 1 _ ->
      t <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> AlteredBeast <$> runMessage msg attrs
