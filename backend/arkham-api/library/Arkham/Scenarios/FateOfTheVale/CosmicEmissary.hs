module Arkham.Scenarios.FateOfTheVale.CosmicEmissary where

import Arkham.Ability
import Arkham.Classes.HasAbilities (extend1)
import Arkham.Classes.HasQueue (push)
import Arkham.Enemy.Types
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (EnemyEvaded, RevealChaosToken)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Window (Window, getMaybeBatchId)

cosmicEmissaryColourAbilities :: EnemyAttrs -> [Ability]
cosmicEmissaryColourAbilities attrs =
  extend1 attrs
    $ forcedAbility attrs 1
    $ oneOf
      [ EnemyAttackedSuccessfully #when Anyone AnySource (be attrs)
      , EnemyEvadedSuccessfully #when Anyone AnySource (be attrs)
      ]

handleCosmicEmissaryColour :: ReverseQueue m => EnemyAttrs -> [Window] -> m ()
handleCosmicEmissaryColour attrs windows = do
  -- The attack path resolves inside a batch we cancel to ward off the hit; the
  -- evade path carries no batch, so only ignore one when present.
  for_ (getMaybeBatchId windows) (push . IgnoreBatch)
  roundModifier (attrs.ability 1) attrs CannotAttack

cosmicEmissaryShatteredAbility :: EnemyAttrs -> ChaosTokenMatcher -> [Ability]
cosmicEmissaryShatteredAbility attrs token =
  extend1 attrs
    $ groupLimit PerTest
      $ restricted
        attrs
        1
        (DuringSkillTest (at_ $ locationWithEnemy attrs.id) <> thisIs attrs (EnemyCanAttack You))
      $ forced
      $ RevealChaosToken #after You token

resolveCosmicEmissaryShatteredAbility :: ReverseQueue m => EnemyAttrs -> InvestigatorId -> m ()
resolveCosmicEmissaryShatteredAbility attrs iid = do
  chooseOneM iid do
    labeledI "automaticallyFailTest" failSkillTest
    labeledI "thisEnemyAttacksYou" $ initiateEnemyAttack attrs (attrs.ability 1) iid
