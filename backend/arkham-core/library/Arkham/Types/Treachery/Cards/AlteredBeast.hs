module Arkham.Types.Treachery.Cards.AlteredBeast
  ( AlteredBeast(..)
  , alteredBeast
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AlteredBeast = AlteredBeast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alteredBeast :: TreacheryCard AlteredBeast
alteredBeast = treachery AlteredBeast Cards.alteredBeast

instance (TreacheryRunner env) => RunMessage env AlteredBeast where
  runMessage msg t@(AlteredBeast attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      abominations <- getSetList @EnemyId Abomination
      t <$ case abominations of
        [] -> pushAll [Surge iid source, Discard $ toTarget attrs]
        [x] -> pushAll
          [ AttachTreachery treacheryId (EnemyTarget x)
          , HealAllDamage (EnemyTarget x)
          ]
        xs -> push
          (chooseOne
            iid
            [ TargetLabel
                (EnemyTarget x)
                [ AttachTreachery treacheryId (EnemyTarget x)
                , HealAllDamage (EnemyTarget x)
                ]
            | x <- xs
            ]
          )
    MoveTo iid lid -> case treacheryAttachedTarget of
      Just (EnemyTarget eid) -> do
        lid' <- getId @LocationId eid
        if lid == lid'
          then
            t <$ push
              (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1)
          else pure t
      _ -> pure t
    EnemyMove eid _ lid | EnemyTarget eid `elem` treacheryAttachedTarget -> do
      iids <- getSetList @InvestigatorId lid
      t <$ pushAll
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
        | iid <- iids
        ]
    _ -> AlteredBeast <$> runMessage msg attrs
