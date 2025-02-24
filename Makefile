.PHONY: bernado_setup bernado_login bernado_start bernado_exit \
	ckmoy_setup ckmoy_login ckmoy_start ckmoy_exit \
	zjunjie_setup zjunjie_login zjunjie_start zjunjie_exit

# ======================
# Julian (bernado) Targets
# ======================

bernado_setup:
	@echo "Setting up container and Git branch for user Julian..."
	@docker run -d -p 8786:localhost:8787 -e PASSWORD=tea --name bernado_urps25 urps25
	@echo "Setting up Git branch"
	@git checkout bernado 2>/dev/null || git checkout -b bernado

bernado_login:
	@echo "Logging into remote environment for user Julian..."
	ssh -tt -L 8786:localhost:8786 -J bernado@evalengin.isr.umich.edu bernado@cochran.isr.umich.edu

bernado_start:
	@echo "Running remote start commands for container Julian..."
	git checkout bernado
	git pull origin bernado
	docker start bernado_urps25
	docker cp ./ bernado_urps25:/home/rstudio/urps-modelling
	docker cp ../../tea/bernado/TEA_2019.csv bernado_urps25:/home/rstudio/TEA_2019.csv


bernado_exit:
	@echo "Performing cleanup and saving changes for container Julian..."
	@rm -rf ../docker
	@mkdir ../docker
	@echo "Copying data from container bernado_urps25..."
	@docker cp bernado_urps25:/home/rstudio/urps-modelling ../docker/urps-modelling
	@docker stop bernado_urps25
	@echo "Updating Git repository on branch 'bernado'..."
	@cd ../docker/urps-modelling &&\
	echo "Moved into the correct directory" &&\
	git checkout bernado &&\
	echo "adding changes" &&\
	git add . &&\
	git commit &&\
	echo "pushing changes" &&\
	git push origin bernado

# ======================
# Caroline (ckmoy) Targets
# ======================

ckmoy_setup:
	@echo "Setting up container and Git branch for user Caroline..."
	@docker run -d -p 8787:localhost:8787 -e PASSWORD=tea --name ckmoy_urps25 urps25
	@echo "Setting up Git branch"
	@git checkout ckmoy 2>/dev/null || git checkout -b ckmoy

ckmoy_login:
	@echo "Logging into remote environment for user Caroline..."
	ssh -tt -L 8787:localhost:8787 -J ckmoy@evalengin.isr.umich.edu ckmoy@cochran.isr.umich.edu

ckmoy_start:
	@echo "Running remote start commands for container Caroline..."
	git checkout ckmoy
	git pull origin ckmoy
	docker start ckmoy_urps25
	docker cp ./ ckmoy_urps25:/home/rstudio/urps-modelling
	docker cp ~/../tea/bernado/TEA_2019.csv ckmoy_urps25:/home/rstudio/TEA_2019.csv

ckmoy_exit:
	@echo "Performing cleanup and saving changes for container ckmoy_urps25..."
	@rm -rf ../docker
	@mkdir ../docker
	@echo "Copying data from container ckmoy_urps25..."
	@docker cp ckmoy_urps25:/home/rstudio/urps-modelling ../docker/urps-modelling
	@docker stop ckmoy_urps25
	@echo "Updating Git repository on branch 'ckmoy'..."
	@cd ../docker/urps-modelling &&\
	git checkout ckmoy &&\
	git add . &&\
	git commit &&\
	git push origin ckmoy &&\
	exit

# ======================
# Junjie (zjunjie) Targets
# ======================

zjunjie_setup:
	@echo "Setting up container and Git branch for user Junjie..."
	@docker run -d -p 8788:localhost:8787 -e PASSWORD=tea --name zjunjie_urps25 urps25
	@echo "Setting up Git branch"
	@git checkout zjunjie 2>/dev/null || git checkout -b zjunjie

zjunjie_login:
	@echo "Logging into remote environment for user Junjie..."
	ssh -tt -L 8788:localhost:8788 -J zjunjie@evalengin.isr.umich.edu zjunjie@cochran.isr.umich.edu

zjunjie_start:
	@echo "Running remote start commands for container Junjie..."
	cd \$HOME/urps-modelling
	git checkout zjunjie
	git pull origin zjunjie
	docker start zjunjie_urps25
	docker cp ./ zjunjie_urps25:/home/rstudio/urps-modelling
	docker cp ~/../tea/bernado/TEA_2019.csv zjunjie_urps25:/home/rstudio/TEA_2019.csv


zjunjie_exit:
	@echo "Performing cleanup and saving changes for container Junjie..."
	@rm -rf ../docker
	@mkdir ../docker
	@echo "Copying data from container zjunjie_urps25..."
	@docker cp zjunjie_urps25:/home/rstudio/urps-modelling ../docker/urps-modelling
	@docker stop zjunjie_urps25
	@echo "Updating Git repository on branch 'zjunjie'..."
	@cd ../docker && \
		git checkout zjunjie && \
		git add .; \
		read -p "Enter commit message: " msg; \
		git commit -m "$$msg"; \
		git push origin zjunjie
	@exit
